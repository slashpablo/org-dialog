;;; org-dialog.el --- Dialog engineering inside Org mode -*- lexical-binding: t; -*-

;; Author: Pablo <contact@slashpablo.com>
;; URL: https://github.com/slashpablo/org-dialog
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.5"))
;; Keywords: tools, convenience, org, ai

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; org-dialog turns any Org file into a Dialog Engineering environment
;; in the spirit of Answer.AI's Solveit.  Prose, executable Babel
;; source blocks, and AI prompts coexist in the same .org document.
;; The AI sees everything above the current prompt and streams its
;; reply back into the buffer as live Org markup, ready to be edited
;; or executed.
;;
;; Talks to Azure OpenAI's chat-completions endpoint.  Configure:
;;
;;   (require 'org-dialog)
;;   (setq org-dialog-azure-resource    "your-resource")    ; subdomain
;;   (setq org-dialog-azure-deployment  "your-deployment")  ; e.g. "gpt-4o"
;;   (setq org-dialog-api-key (getenv "AZURE_OPENAI_API_KEY"))
;;
;; Then in any .org buffer:
;;
;;   M-x org-dialog-mode
;;
;;   #+begin_prompt
;;   What does the function above do?
;;   #+end_prompt
;;
;; With point in the prompt block, hit C-c C-p.  The reply lands in an
;; adjacent #+begin_response ... #+end_response block.  Code in the
;; reply can be copied into native #+begin_src blocks and run with
;; Babel (C-c C-c).
;;
;; To talk to a non-Azure OpenAI-compatible endpoint, set
;; `org-dialog-api-url' directly to the full chat/completions URL.
;;
;; Block names recognised:
;;   prompt    - your question
;;   response  - the AI's reply (auto-inserted)
;;   skip      - excluded from context
;;   pin       - always included in context
;;
;; Keywords:
;;   #+DIALOG_VAR: name   - inject the live value of `name' into context
;;
;; CRAFT.org files in any parent directory of the current buffer are
;; layered into the system context, outermost-first.

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)
(require 'cl-lib)
(require 'subr-x)


;;;; Customization

(defgroup org-dialog nil
  "LLM dialog engineering for Org mode."
  :group 'org
  :prefix "org-dialog-")

(defcustom org-dialog-azure-resource nil
  "Azure OpenAI resource name (the subdomain of openai.azure.com)."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom org-dialog-azure-deployment nil
  "Azure OpenAI deployment name (selects the underlying model)."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom org-dialog-azure-api-version "2024-10-21"
  "Azure OpenAI API version."
  :type 'string)

(defcustom org-dialog-api-url nil
  "Full chat/completions URL.
When nil, derived from the `org-dialog-azure-*' settings.  Set this
directly to talk to any OpenAI-compatible endpoint."
  :type '(choice (const :tag "Derive from azure-* customs" nil) string))

(defcustom org-dialog-api-key nil
  "API key.  When nil, the AZURE_OPENAI_API_KEY env var is read."
  :type '(choice (const :tag "From environment" nil)
                 (string :tag "Literal key")))

(defcustom org-dialog-model nil
  "Optional model identifier.
On Azure OpenAI the deployment name in the URL selects the model, so
this is usually unnecessary.  Set it to override the body's `model'
field for non-Azure endpoints."
  :type '(choice (const :tag "Omit from request" nil) string))

(defcustom org-dialog-max-tokens 4096
  "Maximum tokens to generate per response."
  :type 'integer)

(defcustom org-dialog-max-context-tokens 100000
  "Approximate context budget; emits a warning when exceeded."
  :type 'integer)

(defcustom org-dialog-curl-program "curl"
  "Path to curl, used for streaming HTTP."
  :type 'string)

(defcustom org-dialog-craft-file-name "CRAFT.org"
  "File looked up in parent directories to provide persistent context."
  :type 'string)

(defcustom org-dialog-debug nil
  "When non-nil, log raw payloads to *org-dialog-debug*."
  :type 'boolean)

(defcustom org-dialog-system-prompt
  "You are participating in a notebook-style dialog inside an Emacs Org-mode document.

The document is presented to you with XML-like tags so you can tell the parts apart:
  <craft>     persistent project context loaded from CRAFT.org files
  <note>      prose / explanatory text the user wrote
  <code>      a Babel source block the user wrote
  <output>    the result the user got when they ran that code
  <variable>  a live value evaluated from the user's running session
  <prompt>    a prior question
  <response>  one of your prior replies
  <pin>       content the user explicitly pinned to context

The final <prompt> tag in the document is the message you must answer.

Reply in Markdown that is friendly to Org mode.  When you produce code,
fence it with the language tag (```python, ```elisp, ```sh, ...) so the
user can paste it into a #+begin_src block.

Be terse.  Build on what the dialog already establishes; don't restate it."
  "System prompt prepended to every request."
  :type 'string)


;;;; Tool registry

(defvar org-dialog--tools (make-hash-table :test 'equal)
  "Hash table mapping tool name (string) to a plist of metadata.
Plist keys: :description :input-schema :handler.")

(defun org-dialog-register-tool (&rest args)
  "Register a tool callable by the AI.

ARGS is a plist with keys:
  :name           string
  :description    string
  :input-schema   alist (JSON Schema for the tool input)
  :handler        function taking one alist argument (parsed JSON
                  input) and returning a string."
  (let ((name (plist-get args :name)))
    (unless (stringp name)
      (error "org-dialog-register-tool: :name must be a string"))
    (puthash name
             (list :description (plist-get args :description)
                   :input-schema (plist-get args :input-schema)
                   :handler (plist-get args :handler))
             org-dialog--tools)))

(defun org-dialog-unregister-tool (name)
  "Remove the tool named NAME from the registry."
  (remhash name org-dialog--tools))


;;;; Built-in tools

(defun org-dialog--tool-read-file (args)
  (let* ((path (alist-get 'path args))
         (expanded (and path (expand-file-name path))))
    (cond
     ((null expanded) "Error: missing 'path' argument.")
     ((not (file-readable-p expanded))
      (format "Error: cannot read %s" expanded))
     (t
      (with-temp-buffer
        (insert-file-contents expanded)
        (buffer-string))))))

(defun org-dialog--tool-shell (args)
  (let ((cmd (alist-get 'command args)))
    (if (not (stringp cmd))
        "Error: missing 'command' argument."
      (with-temp-buffer
        (let ((rc (call-process-shell-command cmd nil t t)))
          (format "[exit %s]\n%s" rc (string-trim-right (buffer-string))))))))

(defun org-dialog--tool-eval-elisp (args)
  (let ((expr (alist-get 'expression args)))
    (if (not (stringp expr))
        "Error: missing 'expression' argument."
      (condition-case err
          (format "%S" (eval (read expr) t))
        (error (format "Error: %S" err))))))

(org-dialog-register-tool
 :name "read_file"
 :description "Read the contents of a file from disk."
 :input-schema '((type . "object")
                 (properties
                  . ((path . ((type . "string")
                              (description . "Filesystem path to read.")))))
                 (required . ["path"]))
 :handler #'org-dialog--tool-read-file)

(org-dialog-register-tool
 :name "shell"
 :description "Run a shell command and return its combined output and exit code.  Use sparingly; never run destructive commands without explicit user instruction."
 :input-schema '((type . "object")
                 (properties
                  . ((command . ((type . "string")
                                 (description . "Shell command to execute.")))))
                 (required . ["command"]))
 :handler #'org-dialog--tool-shell)

(org-dialog-register-tool
 :name "eval_elisp"
 :description "Evaluate an Emacs Lisp expression in the user's running Emacs and return the printed result."
 :input-schema '((type . "object")
                 (properties
                  . ((expression . ((type . "string")
                                    (description . "Emacs Lisp expression to evaluate.")))))
                 (required . ["expression"]))
 :handler #'org-dialog--tool-eval-elisp)


;;;; Mode definition

(defvar org-dialog--font-lock-keywords
  '(("^\\([[:space:]]*#\\+begin_prompt\\)\\b"   1 'font-lock-keyword-face t)
    ("^\\([[:space:]]*#\\+end_prompt\\)\\b"     1 'font-lock-keyword-face t)
    ("^\\([[:space:]]*#\\+begin_response\\)\\b" 1 'font-lock-doc-face   t)
    ("^\\([[:space:]]*#\\+end_response\\)\\b"   1 'font-lock-doc-face   t)
    ("^\\(#\\+DIALOG_VAR:.*\\)$"                1 'font-lock-preprocessor-face t)))

(defvar org-dialog-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-p")     #'org-dialog-send)
    (define-key m (kbd "C-c C-x C-c") #'org-dialog-cancel)
    (define-key m (kbd "C-c C-x i")   #'org-dialog-insert-prompt)
    m)
  "Keymap for `org-dialog-mode'.")

;;;###autoload
(define-minor-mode org-dialog-mode
  "Minor mode that adds AI dialog blocks to Org buffers.

Within a buffer that has this mode enabled:
  C-c C-p       Send the prompt block at point.
  C-c C-x C-c   Cancel a streaming response.
  C-c C-x i     Insert a fresh prompt block."
  :lighter " Dialog"
  :keymap org-dialog-mode-map
  (cond
   (org-dialog-mode
    (font-lock-add-keywords nil org-dialog--font-lock-keywords t)
    (font-lock-flush))
   (t
    (font-lock-remove-keywords nil org-dialog--font-lock-keywords)
    (font-lock-flush))))


;;;; Buffer-local streaming state

(defvar-local org-dialog--active-process nil
  "Live curl process when a response is streaming, or nil.")

(defvar-local org-dialog--active-state nil
  "Plist of per-request streaming state.")


;;;; Helpers

(defun org-dialog--debug (fmt &rest args)
  (when org-dialog-debug
    (with-current-buffer (get-buffer-create "*org-dialog-debug*")
      (goto-char (point-max))
      (insert (apply #'format fmt args))
      (insert "\n"))))

(defun org-dialog--special-block-name (el)
  (when (eq (org-element-type el) 'special-block)
    (downcase (or (org-element-property :type el) ""))))

(defun org-dialog--element-content (el)
  "Return literal contents of a special-block EL, trimmed."
  (let ((b (org-element-property :contents-begin el))
        (e (org-element-property :contents-end el)))
    (if (and b e)
        (string-trim (buffer-substring-no-properties b e))
      "")))


;;;; Block detection

(defun org-dialog--current-prompt-block ()
  "Find the prompt special-block enclosing point.
Return (BEGIN END CONTENT), or signal `user-error' if none."
  (let ((el (org-element-at-point)))
    (unless (and (eq (org-element-type el) 'special-block)
                 (string= "prompt" (org-dialog--special-block-name el)))
      (save-excursion
        (let ((case-fold-search t))
          (when (re-search-backward "^[[:space:]]*#\\+begin_prompt\\b" nil t)
            (let ((cand (org-element-at-point)))
              (when (and (eq (org-element-type cand) 'special-block)
                         (string= "prompt"
                                  (org-dialog--special-block-name cand))
                         (>= (org-element-property :end cand) (point)))
                (setq el cand)))))))
    (unless (and (eq (org-element-type el) 'special-block)
                 (string= "prompt" (org-dialog--special-block-name el)))
      (user-error
       "No prompt block at point.  Insert one with `org-dialog-insert-prompt'"))
    (list (org-element-property :begin el)
          (org-element-property :end el)
          (org-dialog--element-content el))))


;;;; Context collection

(defun org-dialog--collect-elements (limit)
  "Walk top-level Org elements from `point-min' up to LIMIT.

Return a flat ordered list of records.  Each record is one of:
  (note STRING)
  (code LANG STRING)
  (output STRING)
  (prompt STRING)
  (response STRING)
  (pin STRING)
  (var NAME)"
  (let ((items '())
        (last-was-code nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) limit) (not (eobp)))
        (let* ((el (org-element-at-point))
               (type (org-element-type el))
               (begin (or (org-element-property :begin el) (point)))
               (end (or (org-element-property :end el) (1+ (point))))
               (clipped-end (min end limit))
               (next-was-code nil))
          (cl-flet ((slice ()
                      (string-trim
                       (buffer-substring-no-properties begin clipped-end))))
            (pcase type
              ('special-block
               (pcase (org-dialog--special-block-name el)
                 ("prompt"
                  (push `(prompt ,(org-dialog--element-content el)) items))
                 ("response"
                  (push `(response ,(org-dialog--element-content el)) items))
                 ("skip" nil)
                 ("pin"
                  (push `(pin ,(org-dialog--element-content el)) items))
                 (_
                  (let ((s (slice)))
                    (when (> (length s) 0)
                      (push `(note ,s) items))))))
              ('src-block
               (let ((lang (org-element-property :language el))
                     (val  (or (org-element-property :value el) "")))
                 (push `(code ,(or lang "") ,(string-trim-right val)) items)
                 (setq next-was-code t)))
              ((or 'fixed-width 'example-block)
               (if last-was-code
                   (push `(output ,(string-trim-right
                                    (or (org-element-property :value el) "")))
                         items)
                 (let ((s (slice)))
                   (when (> (length s) 0)
                     (push `(note ,s) items)))))
              ('keyword
               (let ((key (upcase (or (org-element-property :key el) "")))
                     (val (or (org-element-property :value el) "")))
                 (cond
                  ((string= key "DIALOG_VAR")
                   (push `(var ,(string-trim val)) items))
                  ((string-prefix-p "RESULTS" key) nil)
                  (t nil))))
              ((or 'paragraph 'plain-list 'quote-block 'table 'verse-block)
               (let ((s (slice)))
                 (when (> (length s) 0)
                   (push `(note ,s) items))))
              ('headline
               (push `(note ,(buffer-substring-no-properties
                              begin (min (line-end-position) limit)))
                     items))
              (_ nil)))
          (setq last-was-code next-was-code)
          (let ((p (point)))
            (goto-char (max clipped-end (1+ p)))))))
    (nreverse items)))


;;;; CRAFT files

(defun org-dialog--find-craft-files (start-dir)
  "Return CRAFT.org paths walking from START-DIR up to the filesystem root.
Outermost first, so they layer top-down."
  (let ((result '())
        (dir (file-name-as-directory (or start-dir default-directory))))
    (while dir
      (let ((candidate (expand-file-name org-dialog-craft-file-name dir)))
        (when (file-readable-p candidate)
          (push candidate result)))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (and parent (not (string= parent dir)) parent))))
    result))

(defun org-dialog--craft-content ()
  "Concatenated <craft>-tagged content of CRAFT.org files for this buffer."
  (let* ((start (and buffer-file-name
                     (file-name-directory buffer-file-name)))
         (files (org-dialog--find-craft-files start))
         (parts '()))
    (dolist (f files)
      (push (format "<craft path=%S>\n%s\n</craft>"
                    (abbreviate-file-name f)
                    (with-temp-buffer
                      (insert-file-contents f)
                      (string-trim (buffer-string))))
            parts))
    (mapconcat #'identity (nreverse parts) "\n\n")))


;;;; Variable injection

(defun org-dialog--inject-variable (name)
  "Best-effort evaluate NAME and return a string representation."
  (condition-case _err
      (let ((sym (intern-soft name)))
        (cond
         ((and sym (boundp sym))
          (format "%S" (symbol-value sym)))
         (t
          (format "%S" (eval (read name) t)))))
    (error (format "<error evaluating %s>" name))))


;;;; Render context to a single user message

(defun org-dialog--render-record (rec)
  (pcase rec
    (`(note ,s)
     (let ((trimmed (string-trim s)))
       (when (> (length trimmed) 0)
         (format "<note>\n%s\n</note>" trimmed))))
    (`(code ,lang ,s)
     (format "<code lang=\"%s\">\n%s\n</code>" (or lang "") s))
    (`(output ,s)
     (format "<output>\n%s\n</output>" s))
    (`(prompt ,s)
     (format "<prompt>\n%s\n</prompt>" s))
    (`(response ,s)
     (format "<response>\n%s\n</response>" s))
    (`(pin ,s)
     (format "<pin>\n%s\n</pin>" s))
    (`(var ,name)
     (format "<variable name=\"%s\">\n%s\n</variable>"
             name (org-dialog--inject-variable name)))))

(defun org-dialog--build-user-content (records prompt-text)
  "Compose a single user-message string from RECORDS and the current PROMPT-TEXT."
  (let* ((rendered (delq nil (mapcar #'org-dialog--render-record records)))
         (craft (org-dialog--craft-content))
         (parts (append (and (> (length craft) 0) (list craft))
                        rendered
                        (list (format "<prompt>\n%s\n</prompt>" prompt-text)))))
    (mapconcat #'identity parts "\n\n")))


;;;; Tool schemas for API (OpenAI function-calling shape)

(defun org-dialog--tools-payload ()
  "Return a vector of tool schemas to send to the API."
  (let (tools)
    (maphash
     (lambda (name plist)
       (push `((type . "function")
               (function . ((name . ,name)
                            (description . ,(plist-get plist :description))
                            (parameters . ,(plist-get plist :input-schema)))))
             tools))
     org-dialog--tools)
    (apply #'vector (nreverse tools))))

(defun org-dialog--invoke-tool (name input)
  "Run the registered tool NAME with INPUT (alist).  Return string result."
  (let ((entry (gethash name org-dialog--tools)))
    (if (null entry)
        (format "Tool %s is not registered." name)
      (condition-case err
          (funcall (plist-get entry :handler) input)
        (error (format "Tool %s raised: %S" name err))))))


;;;; Response block management

(defun org-dialog--ensure-response-block (after-pos)
  "Ensure a fresh response block immediately follows AFTER-POS.

AFTER-POS is the :end position of the prompt block.  If a response block
already follows, its contents are cleared.  Return a marker positioned
at the body of the response block (between the two tag lines)."
  (save-excursion
    (goto-char after-pos)
    (skip-chars-forward " \t\n")
    (let ((case-fold-search t))
      (when (looking-at "[[:space:]]*#\\+begin_response\\b")
        (let ((el (org-element-at-point)))
          (when (and el (eq (org-element-type el) 'special-block))
            (delete-region (org-element-property :begin el)
                           (org-element-property :end el))))))
    (let ((insertion-start (point)))
      (insert "#+begin_response\n\n#+end_response\n\n")
      (goto-char insertion-start)
      (forward-line 1)
      (copy-marker (point) t))))


;;;; Streaming insertion

(defun org-dialog--insert-at-marker (state text)
  "Insert TEXT at STATE's :marker, in STATE's :buffer."
  (let ((buf (plist-get state :buffer))
        (marker (plist-get state :marker)))
    (when (and (buffer-live-p buf) marker (marker-position marker))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char marker)
            (insert text)
            (set-marker marker (point))))))))


;;;; SSE parser (OpenAI chat-completions stream)

(defun org-dialog--accumulate-tool-call (state idx id name args)
  "Accumulate a streaming tool_call delta at IDX into STATE."
  (let* ((tcs (plist-get state :tool-calls))
         (existing (cl-find idx tcs
                            :key (lambda (tc) (plist-get tc :index)))))
    (cond
     (existing
      (when id   (plist-put existing :id id))
      (when name (plist-put existing :name name))
      (when args (plist-put existing :arguments
                            (concat (or (plist-get existing :arguments) "")
                                    args))))
     (t
      (plist-put state :tool-calls
                 (append tcs
                         (list (list :index idx
                                     :id id
                                     :name name
                                     :arguments (or args "")))))))))

(defun org-dialog--handle-sse-event (state event-text)
  "Process one SSE event block."
  (let (data-line)
    (dolist (line (split-string event-text "\n"))
      (when (string-prefix-p "data: " line)
        (setq data-line (substring line 6))))
    (when (and data-line (not (string= data-line "[DONE]")))
      (org-dialog--debug "<< %s" data-line)
      (let ((payload (condition-case nil
                         (json-parse-string data-line
                                            :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil)
                       (error nil))))
        (when payload
          (let* ((err (alist-get 'error payload))
                 (choices (alist-get 'choices payload))
                 (choice (car choices))
                 (delta (alist-get 'delta choice))
                 (finish (alist-get 'finish_reason choice))
                 (content (alist-get 'content delta))
                 (tcs (alist-get 'tool_calls delta)))
            (cond
             (err
              (let ((msg (or (alist-get 'message err) "unknown error")))
                (org-dialog--insert-at-marker
                 state (format "\n[org-dialog API error: %s]\n" msg))))
             (t
              (when (and content (stringp content) (> (length content) 0))
                (org-dialog--insert-at-marker state content)
                (plist-put state :assistant-text
                           (concat (or (plist-get state :assistant-text) "")
                                   content)))
              (when tcs
                (dolist (tc tcs)
                  (let* ((idx  (alist-get 'index tc))
                         (id   (alist-get 'id tc))
                         (fn   (alist-get 'function tc))
                         (name (and fn (alist-get 'name fn)))
                         (args (and fn (alist-get 'arguments fn))))
                    (org-dialog--accumulate-tool-call state idx id name args))))
              (when finish
                (plist-put state :finish-reason finish))))))))))

(defun org-dialog--make-process-filter (state)
  (lambda (proc chunk)
    (when (process-live-p proc)
      (let* ((prev (or (plist-get state :sse-buffer) ""))
             (full (concat prev chunk))
             (events (split-string full "\n\n")))
        (plist-put state :sse-buffer (car (last events)))
        (dolist (ev (butlast events))
          (when (> (length (string-trim ev)) 0)
            (org-dialog--handle-sse-event state ev)))))))


;;;; Tool roundtrip

(defun org-dialog--make-assistant-message (state)
  "Build the assistant message reflecting the streamed reply."
  (let* ((text (or (plist-get state :assistant-text) ""))
         (tcs (plist-get state :tool-calls))
         (tool-calls-vec
          (apply #'vector
                 (mapcar
                  (lambda (tc)
                    `((id . ,(plist-get tc :id))
                      (type . "function")
                      (function . ((name . ,(plist-get tc :name))
                                   (arguments . ,(or (plist-get tc :arguments) ""))))))
                  tcs))))
    `((role . "assistant")
      (content . ,(if (> (length text) 0) text ""))
      ,@(when (> (length tool-calls-vec) 0)
          `((tool_calls . ,tool-calls-vec))))))

(defun org-dialog--run-pending-tools (state)
  "Execute streamed tool_calls.  Return a vector of role=tool messages."
  (let* ((tcs (plist-get state :tool-calls))
         results)
    (dolist (tc tcs)
      (let* ((id   (plist-get tc :id))
             (name (plist-get tc :name))
             (raw  (or (plist-get tc :arguments) ""))
             (input (if (> (length raw) 0)
                        (condition-case nil
                            (json-parse-string raw
                                               :object-type 'alist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object nil)
                          (error nil))
                      nil)))
        (org-dialog--insert-at-marker
         state (format "\n\n[tool: %s %s]\n" name (if (> (length raw) 0) raw "{}")))
        (let ((output (org-dialog--invoke-tool name input)))
          (org-dialog--insert-at-marker
           state (format "[result]\n%s\n\n" (string-trim-right output)))
          (push `((role . "tool")
                  (tool_call_id . ,id)
                  (content . ,output))
                results))))
    (apply #'vector (nreverse results))))


;;;; Sentinel

(defun org-dialog--make-sentinel (state)
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (let* ((buf (plist-get state :buffer))
             (finish (plist-get state :finish-reason))
             (stderr-buf (process-buffer proc)))
        (cond
         ((string= finish "tool_calls")
          (let* ((assistant-msg (org-dialog--make-assistant-message state))
                 (tool-results (org-dialog--run-pending-tools state))
                 (next-messages
                  (vconcat
                   (plist-get state :messages)
                   (vector assistant-msg)
                   tool-results)))
            (plist-put state :messages next-messages)
            (plist-put state :sse-buffer "")
            (plist-put state :assistant-text "")
            (plist-put state :tool-calls nil)
            (plist-put state :finish-reason nil)
            (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
            (org-dialog--launch-request state)))
         (t
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq org-dialog--active-process nil
                    org-dialog--active-state nil)))
          (when (and (string-match-p "exited abnormally" event)
                     (not (member finish '("stop" "length" "content_filter"))))
            (let ((stderr (and (buffer-live-p stderr-buf)
                               (with-current-buffer stderr-buf
                                 (string-trim (buffer-string))))))
              (org-dialog--insert-at-marker
               state
               (format "\n[org-dialog: %s%s]"
                       (string-trim event)
                       (if (and stderr (> (length stderr) 0))
                           (format " — %s" stderr)
                         "")))))
          (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
          (message "org-dialog: done (%s)"
                   (or finish (string-trim event)))))))))


;;;; Request

(defun org-dialog--api-key ()
  (or org-dialog-api-key
      (getenv "AZURE_OPENAI_API_KEY")
      (user-error
       "No API key set.  Customize `org-dialog-api-key' or export AZURE_OPENAI_API_KEY")))

(defun org-dialog--effective-url ()
  "Return the chat/completions URL to POST to."
  (cond
   ((and (stringp org-dialog-api-url)
         (> (length org-dialog-api-url) 0))
    org-dialog-api-url)
   ((and org-dialog-azure-resource org-dialog-azure-deployment)
    (format
     "https://%s.openai.azure.com/openai/deployments/%s/chat/completions?api-version=%s"
     org-dialog-azure-resource
     org-dialog-azure-deployment
     org-dialog-azure-api-version))
   (t
    (user-error
     "Set `org-dialog-azure-resource' and `org-dialog-azure-deployment', or `org-dialog-api-url' directly"))))

(defun org-dialog--launch-request (state)
  "Launch a streaming curl request from STATE."
  (let* ((tools (plist-get state :tools))
         (body `((messages . ,(plist-get state :messages))
                 (max_tokens . ,org-dialog-max-tokens)
                 (stream . t)
                 ,@(when (and (stringp org-dialog-model)
                              (> (length org-dialog-model) 0))
                     `((model . ,org-dialog-model)))
                 ,@(when (and tools (> (length tools) 0))
                     `((tools . ,tools)))))
         (json-body (let ((json-encoding-pretty-print nil))
                      (json-encode body)))
         (key (org-dialog--api-key))
         (url (org-dialog--effective-url))
         (proc (make-process
                :name "org-dialog"
                :buffer (generate-new-buffer " *org-dialog-stderr*")
                :command (list org-dialog-curl-program
                               "-sS" "-N"
                               "-X" "POST"
                               "-H" "content-type: application/json"
                               "-H" (format "api-key: %s" key)
                               "-d" "@-"
                               url)
                :connection-type 'pipe
                :noquery t
                :filter (org-dialog--make-process-filter state)
                :sentinel (org-dialog--make-sentinel state))))
    (org-dialog--debug ">> %s\nPOST %s" json-body url)
    (let ((buf (plist-get state :buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq org-dialog--active-process proc))))
    (process-send-string proc json-body)
    (process-send-eof proc)))


;;;; Public commands

;;;###autoload
(defun org-dialog-send ()
  "Send the prompt block at point to the AI and stream the reply back."
  (interactive)
  (unless org-dialog-mode
    (org-dialog-mode 1))
  (when (and org-dialog--active-process
             (process-live-p org-dialog--active-process))
    (user-error "A response is already streaming.  Cancel with `C-c C-x C-c'"))
  (pcase-let* ((`(,pbeg ,pend ,ptext) (org-dialog--current-prompt-block))
               (records (org-dialog--collect-elements pbeg))
               (user-content (org-dialog--build-user-content records ptext))
               (estimated-tokens (/ (length user-content) 4))
               (messages (vector
                          `((role . "system")
                            (content . ,org-dialog-system-prompt))
                          `((role . "user")
                            (content . ,user-content))))
               (tools (org-dialog--tools-payload))
               (marker (org-dialog--ensure-response-block pend))
               (state (list :buffer (current-buffer)
                            :marker marker
                            :messages messages
                            :tools tools
                            :assistant-text ""
                            :tool-calls nil
                            :finish-reason nil
                            :sse-buffer "")))
    (when (> estimated-tokens org-dialog-max-context-tokens)
      (message "org-dialog: context ~%d tokens, exceeds budget %d"
               estimated-tokens org-dialog-max-context-tokens))
    (setq org-dialog--active-state state)
    (org-dialog--launch-request state)
    (message "org-dialog: streaming...")))

;;;###autoload
(defun org-dialog-cancel ()
  "Cancel the in-flight streaming request, if any."
  (interactive)
  (when (and org-dialog--active-process
             (process-live-p org-dialog--active-process))
    (delete-process org-dialog--active-process))
  (setq org-dialog--active-process nil
        org-dialog--active-state nil)
  (message "org-dialog: cancelled"))

;;;###autoload
(defun org-dialog-insert-prompt ()
  "Insert an empty prompt block at point and place point inside it."
  (interactive)
  (unless (bolp) (insert "\n"))
  (let ((p (point)))
    (insert "#+begin_prompt\n\n#+end_prompt\n")
    (goto-char (+ p (length "#+begin_prompt\n")))))


(provide 'org-dialog)

;;; org-dialog.el ends here
