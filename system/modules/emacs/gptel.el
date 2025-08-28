(use-package gptel
  :config
  (setq gptel-log-level 'debug
  gptel-default-mode 'org-mode
  gptel-cache nil)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "＠🧑🏻‍🍳\n"
  (alist-get 'org-mode gptel-response-prefix-alist) "＠🤖\n")

  (setq gptel-directives
  '((default . "Use the read_memory_node_by_id tool to read memory id 27E07272-DCC1-4A18-851A-1B0F297F5A60. This will initlize context for your personality, the human user, and the availible tools. Do this before beginning to respond to the human user's first request.")
    (nomemory . "The LLM (you) is a helpful assistant integrated into emacs. The LLM writes in org markup, using org source blocks when writing code.")))

  (defun my/get-api-key (host)
    (when-let ((auth (car (auth-source-search
		     :host host
		     :require '(:secret)))))
(let ((token (plist-get auth :secret)))
  (if (functionp token)
      (funcall token)
    token))))

  (gptel-make-anthropic "Claude"
		  :stream t
		  :key (lambda () (my/get-api-key "api.anthropic.com")))

  (gptel-make-gemini "Gemini"
	       :key (lambda () (my/get-api-key "api.gemini.com")))

  (gptel-make-openai "ChatGPT"
	       :key (lambda () (my/get-api-key "api.openai.com"))
	       :stream t
	       :models gptel--openai-models))

				  ;(setq gptel-backend (gptel-make-gh-copilot "Copilot")
				  ;      gptel-model 'gpt-4o-mini))

(defun my/gptel-cache-one-shot ()
  (interactive)
  (setq gptel-cache '(system tool)))

(defun my/gptel-cache-multi-shot ()
  (interactive)
  (setq gptel-cache t))

(defun my/gptel-cache-off ()
  (interactive)
  (setq gptel-cache nil))

;; Context minification function for GPTel
(defun my/gptel-minify-context ()
  "Minify the current gptel chat buffer context to reduce tokens."
  (interactive)
  (when (derived-mode-p 'gptel-mode)
    (let ((inhibit-read-only t)
    (modified (buffer-modified-p)))
(save-excursion
  ;; Remove excess blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n+" nil t)
    (replace-match "\n\n"))

  ;; Collapse code blocks to show minimal context
  (goto-char (point-min))
  (while (re-search-forward "```\\([^`\n]*\\)\n\\([^`]*?\\)\n```" nil t)
    (let* ((lang (match-string 1))
	   (code (match-string 2))
	   (lines (split-string code "\n"))
	   (total-lines (length lines))
	   (preview-lines 3)
	   (minified-code
	    (if (> total-lines (* 2 preview-lines))
		(concat
		 (string-join (seq-take lines preview-lines) "\n")
		 "\n... "
		 (number-to-string (- total-lines (* 2 preview-lines)))
		 " lines collapsed ...\n"
		 (string-join (seq-take-last preview-lines lines) "\n"))
	      code)))
      (replace-match (format "```%s\n%s\n```" lang minified-code))))

  ;; Optionally truncate very long responses
  (goto-char (point-min))
  (while (re-search-forward "^Assistant: \\([^\n]*\\(?:\n[^\n]+\\)*\\)" nil t)
    (let* ((response (match-string 1))
	   (lines (split-string response "\n"))
	   (max-lines 20))
      (when (> (length lines) max-lines)
	(let ((truncated-response
	       (concat
		(string-join (seq-take lines (/ max-lines 2)) "\n")
		"\n... "
		(number-to-string (- (length lines) max-lines))
		" lines summarized ...\n"
		(string-join (seq-take-last (/ max-lines 2) lines) "\n"))))
	  (replace-match (concat "Assistant: " truncated-response))))))

  ;; Remove trailing whitespace
  (delete-trailing-whitespace))

;; Restore modification state
(set-buffer-modified-p modified))

    ;; Provide feedback on reduction
    (message "Context minified. Use M-x revert-buffer to restore if needed.")))

;; Bind minification function in gptel-mode

(setq gptel-use-tools t
gptel-tools nil)  

(defun register-gptel-tool (tool-name)
  "Register a tool with gptel by its NAME."
  (add-to-list 'gptel-tools (gptel-get-tool tool-name)))

;; Make sure repomix is available
;;(unless (executable-find "repomix")
;;(message "Warning: repomix not found in PATH. The repomix tool won't work until installed."))


(defvar my/file-bookmarks
  '(("emacs config" . (:path "~/nix/system/with/user/with/program/emacs.org"
		       :description "My literate org based emacs configuration"))
    ("inbox" . (:path "~/notes/inbox.org"
		:description "My inbox for my TODOs and notes"))
    ))

(require 'json)

(defun my/gptel-run-shell-command (command)
  "Run COMMAND in the shell and return a JSON string with `exit_code` and `output`."
  (let* ((buffer (generate-new-buffer " *gptel-shell-output*"))
   (exit-code (call-process-shell-command command nil buffer t))
   (output
    (with-current-buffer buffer
      (prog1
	  (buffer-string)
	(kill-buffer)))))
    (json-encode `(("exit_code" . ,exit-code)
	     ("output"    . ,output)))))

;; Register it as a gptel tool
(gptel-make-tool
 :name        "shell_command"
 :function    #'my/gptel-run-shell-command
 :description "Run an arbitrary shell COMMAND and return JSON with exit_code and output."
 :args        '((:name "command"
		 :type string
		 :description "The shell command to execute (as you would type it in a terminal)"))
 :category    "system")

(register-gptel-tool "shell_command")

(defun my/gptel-execute-elisp (elisp-code)
  "Execute arbitrary elisp CODE and return the result as a string.
The code is evaluated in the current Emacs process."
  (condition-case err
      (if (null elisp-code)
          (json-encode `(("success" . nil)
                        ("error" . "No elisp code provided")))
        (let* ((result (eval (read elisp-code) t))
               (result-string (format "%S" result)))
          (json-encode `(("success" . t)
                        ("result" . ,result-string)))))
    (error
     (json-encode `(("success" . nil)
                   ("error" . ,(error-message-string err)))))))

(gptel-make-tool
 :name        "execute_elisp"
 :confirm t
 :include t
 :function    #'my/gptel-execute-elisp
 :description "Execute arbitrary elisp code in the current Emacs process and return the result.
	 Use with extreme caution as this has full access to the Emacs environment."
 :args        '((:name "code"
		 :type string
		 :description "The elisp code to evaluate"))
 :category    "development")

(register-gptel-tool "execute_elisp")

(require 'json)

;; Buffer-Based File Editor - Reliable and Emacs-Native
;; Uses buffer operations instead of text manipulation for better reliability

(defun my/buffer-search-replace (search replace &optional count case-sensitive)
  "Perform search and replace in current buffer."
  (let ((case-fold-search (not case-sensitive))
        (replacements 0)
        (max-replacements (or count most-positive-fixnum)))
    (goto-char (point-min))
    (while (and (< replacements max-replacements)
                (search-forward search nil t))
      (replace-match replace nil t)
      (setq replacements (1+ replacements)))
    replacements))

(defun my/buffer-goto-line-insert (line-number text &optional position)
  "Go to line and insert text. POSITION can be 'beginning, 'end, or 'after."
  (goto-char (point-min))
  (forward-line (1- line-number))
  (pcase (or position 'end)
    ('beginning (beginning-of-line))
    ('end (end-of-line))
    ('after (end-of-line) (forward-line 1) (beginning-of-line)))
  (when (eq position 'after)
    (open-line 1))
  (insert text)
  (when (eq position 'after)
    (forward-line -1)))

(defun my/buffer-replace-region (start-line end-line new-content)
  "Replace content between START-LINE and END-LINE with NEW-CONTENT."
  (goto-char (point-min))
  (forward-line (1- start-line))
  (let ((start-pos (line-beginning-position)))
    (forward-line (- end-line start-line))
    (let ((end-pos (line-end-position)))
      (delete-region start-pos end-pos)
      (goto-char start-pos)
      (insert new-content))))

(defun my/buffer-find-function-replace (function-name new-content &optional language)
  "Find function by name and replace with new content."
  (let ((search-pattern
         (pcase (or language
                    (pcase major-mode
                      ('python-mode "python")
                      ('emacs-lisp-mode "elisp")
                      ('js-mode "javascript")
                      ('typescript-mode "typescript")
                      (_ "generic")))
           ("python" (format "def %s(" function-name))
           ("elisp" (format "(defun %s " function-name))
           ("javascript" (format "function %s(" function-name))
           ("typescript" (format "function %s(" function-name))
           (_ (format "%s" function-name)))))
    (goto-char (point-min))
    (when (search-forward search-pattern nil t)
      (pcase major-mode
        ('python-mode
         (python-nav-beginning-of-defun)
         (let ((start (point)))
           (python-nav-end-of-defun)
           (delete-region start (point))
           (goto-char start)
           (insert new-content)))
        ('emacs-lisp-mode
         (beginning-of-defun)
         (let ((start (point)))
           (end-of-defun)
           (delete-region start (point))
           (goto-char start)
           (insert new-content)))
        (_
         ;; Generic approach - find likely function boundaries
         (let ((start (line-beginning-position)))
           (forward-sexp) ; Try to skip function
           (delete-region start (point))
           (goto-char start)
           (insert new-content))))
      t)))

(defun my/buffer-add-import (import-statement &optional language)
  "Add import statement to appropriate location in file."
  (let ((language (or language
                      (pcase major-mode
                        ('python-mode "python")
                        ('js-mode "javascript")
                        ('typescript-mode "typescript")
                        (_ "generic")))))
    (goto-char (point-min))
    (pcase language
      ("python"
       ;; Find last import or add after shebang/docstring
       (let ((insert-pos (point-min)))
         (while (or (looking-at "^#!") ;; shebang
                    (looking-at "^\"\"\".*\"\"\"$") ;; docstring
                    (looking-at "^'''.*'''$") ;; docstring
                    (looking-at "^[[:space:]]*$") ;; empty
                    (looking-at "^from .*import") ;; import
                    (looking-at "^import "))
           (forward-line)
           (setq insert-pos (point)))
         (goto-char insert-pos)
         (unless (bolp) (insert "\n"))
         (insert import-statement "\n")))
      (_
       ;; Generic - add at top after any comments
       (while (looking-at "^[[:space:]]*\\(#\\|//\\|/\\*\\)")
         (forward-line))
       (insert import-statement "\n")))))

(defun my/gptel-buffer-editor (file-path operation arguments)
  "Edit FILE-PATH using Emacs buffer operations.
  
OPERATION can be:
  - 'search-replace: arguments should be JSON [search, replace, count?, case-sensitive?]
  - 'goto-line-insert: arguments should be JSON [line-number, text, position?] 
  - 'replace-region: arguments should be JSON [start-line, end-line, new-content]
  - 'find-function-replace: arguments should be JSON [function-name, new-content, language?]
  - 'add-import: arguments should be JSON [import-statement, language?]
  - 'indent-region: arguments should be JSON [start-line, end-line]
  
Returns JSON with success status and details."
  (let ((file-path (expand-file-name file-path))
        (backup-file (make-temp-file "gptel-backup")))
    
    ;; Validate file exists
    (unless (file-exists-p file-path)
      (error "File does not exist: %s" file-path))
    
    ;; Create backup
    (copy-file file-path backup-file t)
    
    (condition-case err
        (with-current-buffer (find-file-noselect file-path)
          (let ((modified-p (buffer-modified-p))
                (result nil))
            
            ;; Parse arguments from JSON string
            (let ((parsed-args (if (stringp arguments)
                                  (json-parse-string arguments :array-type 'list)
                                arguments)))
              
              ;; Perform the operation
              (setq result
                    (pcase (intern operation)
                      ('search-replace
                       (let* ((search (nth 0 parsed-args))
                              (replace (nth 1 parsed-args))
                              (count (nth 2 parsed-args))
                              (case-sensitive (nth 3 parsed-args))
                              (replacements (my/buffer-search-replace search replace count case-sensitive)))
                         (format "Replaced %d occurrences" replacements)))
                    
                      ('goto-line-insert
                       (let ((line-number (nth 0 parsed-args))
                             (text (nth 1 parsed-args))
                             (position (when (nth 2 parsed-args) (intern (nth 2 parsed-args)))))
                         (my/buffer-goto-line-insert line-number text position)
                         "Text inserted at specified line"))
                    
                      ('replace-region
                       (let ((start-line (nth 0 parsed-args))
                             (end-line (nth 1 parsed-args))
                             (new-content (nth 2 parsed-args)))
                         (my/buffer-replace-region start-line end-line new-content)
                         "Region replaced successfully"))
                    
                      ('find-function-replace
                       (let ((function-name (nth 0 parsed-args))
                             (new-content (nth 1 parsed-args))
                             (language (nth 2 parsed-args)))
                         (if (my/buffer-find-function-replace function-name new-content language)
                             "Function replaced successfully"
                           "Function not found")))
                    
                      ('add-import
                       (let ((import-statement (nth 0 parsed-args))
                             (language (nth 1 parsed-args)))
                         (my/buffer-add-import import-statement language)
                         "Import statement added"))
                    
                      ('indent-region
                       (let ((start-line (nth 0 parsed-args))
                             (end-line (nth 1 parsed-args)))
                         (goto-char (point-min))
                         (forward-line (1- start-line))
                         (let ((start (point)))
                           (forward-line (- end-line start-line))
                           (indent-region start (point)))
                         "Region indented"))
                    
                      (_ (error "Unknown operation: %s" operation)))))
            
            ;; Save if buffer was modified
            (when (buffer-modified-p)
              (save-buffer)
              (message "Saved %s" file-path))
            
            ;; Clean up backup on success
            (delete-file backup-file)
            
            ;; Return success
            (json-encode `(("success" . t)
                          ("operation" . ,(symbol-name operation))
                          ("file" . ,file-path)
                          ("result" . ,result)))))
      
      ;; Error handling - restore backup
      (error
       (when (file-exists-p backup-file)
         (copy-file backup-file file-path t)
         (delete-file backup-file)
         ;; Reload buffer if it exists
         (when-let ((buf (get-file-buffer file-path)))
           (with-current-buffer buf
             (revert-buffer t t))))
       
       (json-encode `(("success" . nil)
                     ("operation" . ,(symbol-name operation))
                     ("file" . ,file-path)
                     ("error" . ,(error-message-string err))))))))

;; Register the buffer-based file editor tool with GPTel
(gptel-make-tool
 :name "buffer_editor"
 :function #'my/gptel-buffer-editor
 :description "Reliable file editing using Emacs buffer operations. 
Operations: search-replace, goto-line-insert, replace-region, find-function-replace, add-import, indent-region.
More reliable than text-based editing approaches."
 :args '((:name "file_path" 
          :type string
          :description "Path to the file to edit")
         (:name "operation" 
          :type string  
          :description "Operation: search-replace, goto-line-insert, replace-region, find-function-replace, add-import, indent-region")
         (:name "arguments"
          :type string
          :description "JSON array of arguments for the operation. See function documentation for argument structure per operation."))
 :category "file-editing")

(register-gptel-tool "buffer_editor")
(message "buffer_editor tool loaded")

(require 'json)

;; GPTel File Editing Tool - Enhanced Implementation
;; Based on design document and testing feedback

;; Helper functions for different edit modes

(defun my/apply-diff-edit-original (file-path diff-content)
  "Original simple diff application - kept for reference."
  (let ((diff-file (make-temp-file "gptel-diff" nil ".patch")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert diff-content)
            (write-region (point-min) (point-max) diff-file))
          (let ((default-directory (file-name-directory file-path)))
            (shell-command (format "patch -p0 < %s" diff-file))))
      (when (file-exists-p diff-file)
        (delete-file diff-file)))))

;; CRITICAL BUG FIX: Previous version using diff-mode was silently failing
;; while reporting success. This version uses patch command with proper error handling.
(defun my/apply-diff-edit (file-path diff-content)
  "Apply diff using patch command with proper error handling."
  (let ((diff-file (make-temp-file "gptel-diff" nil ".patch"))
        (file-dir (file-name-directory file-path))
        (file-name (file-name-nondirectory file-path)))
    (unwind-protect
        (progn
          ;; Write diff to temporary file
          (with-temp-buffer
            (insert diff-content)
            (write-region (point-min) (point-max) diff-file))
          
          ;; Apply patch with proper error handling
          (let ((default-directory file-dir)
                (cmd (format "patch -p0 %s < %s" file-name diff-file)))
            (if (= 0 (call-process-shell-command cmd nil nil nil))
                (format "Applied diff successfully using: %s" cmd)
              (error "Patch command failed: %s" cmd))))
      
      ;; Cleanup
      (when (file-exists-p diff-file)
        (delete-file diff-file)))))
(defun my/apply-search-replace-edit (file-path operations-json)
  "Apply search/replace operations to FILE-PATH from OPERATIONS-JSON."
  (let ((operations (json-parse-string operations-json :array-type 'list :object-type 'hash-table)))
    (with-temp-buffer
      (insert-file-contents file-path)
      (dolist (op operations)
        (let ((search (gethash "search" op))
              (replace (gethash "replace" op))
              (count (gethash "count" op)))
          (goto-char (point-min))
          (if count
              (let ((case-fold-search nil))
                (while (and (> count 0) (search-forward search nil t))
                  (replace-match replace nil t)
                  (setq count (1- count))))
            (while (search-forward search nil t)
              (replace-match replace nil t)))))
      (write-region (point-min) (point-max) file-path))))

(defun my/apply-lines-edit (file-path operations-json)
  "Apply line-based operations with improved reliability."
  (let ((operations (json-parse-string operations-json :array-type 'list :object-type 'hash-table))
        (lines-list nil))
    (with-temp-buffer
      (insert-file-contents file-path)
      ;; Convert to list of lines for easier manipulation
      (setq lines-list (split-string (buffer-string) "\n"))
      
      ;; Sort operations by line number (descending) to avoid shifting issues
      (setq operations (sort operations 
                           (lambda (a b) 
                             (> (gethash "line" a 0) (gethash "line" b 0)))))
      
      ;; Apply operations from bottom up
      (dolist (op operations)
        (let ((action (gethash "action" op))
              (line-num (gethash "line" op))
              (content (gethash "content" op))
              (end-line (gethash "end_line" op)))
          
          ;; Validate line numbers
          (when (or (<= line-num 0) (> line-num (1+ (length lines-list))))
            (error "Line number %d out of bounds (file has %d lines)" 
                   line-num (length lines-list)))
          
          (pcase action
            ("insert"
             ;; Insert content at specified line
             (setq lines-list 
                   (append (seq-take lines-list (1- line-num))
                           (list content)
                           (seq-drop lines-list (1- line-num)))))
            ("delete"
             ;; Delete line range
             (let ((start (max 1 line-num))
                   (end (min (or end-line line-num) (length lines-list))))
               (setq lines-list 
                     (append (seq-take lines-list (1- start))
                             (seq-drop lines-list end)))))
            ("replace"
             ;; Replace single line
             (when (<= line-num (length lines-list))
               (setf (nth (1- line-num) lines-list) content))))))
      
      ;; Write back the modified lines
      (erase-buffer)
      (insert (string-join lines-list "\n"))
      (write-region (point-min) (point-max) file-path))))

(defun my/gptel-file-editor (file-path edit-mode edit-data)
  "Comprehensive file editing function supporting multiple modes.
FILE-PATH: Path to the file to edit
EDIT-MODE: One of 'diff', 'search-replace', 'lines'  
EDIT-DATA: Edit instructions based on the mode"
  (let* ((file-path (expand-file-name file-path))
         (backup-file (concat file-path ".gptel-backup-" 
                            (format-time-string "%Y%m%d%H%M%S")))
         (result nil))
    
    ;; Validate file exists and is writable
    (unless (file-exists-p file-path)
      (error "File does not exist: %s" file-path))
    (unless (file-writable-p file-path)
      (error "File is not writable: %s" file-path))
    
    ;; Create backup
    (copy-file file-path backup-file t)
    
    ;; Apply edit based on mode
    (condition-case err
        (progn
          (pcase edit-mode
            ("diff" 
             (my/apply-diff-edit file-path edit-data)
             (setq result (format "Applied diff to %s" file-path)))
            ("search-replace" 
             (my/apply-search-replace-edit file-path edit-data)
             (setq result (format "Applied search/replace operations to %s" file-path)))
            ("lines" 
             (my/apply-lines-edit file-path edit-data)
             (setq result (format "Applied line operations to %s" file-path)))
            (_ (error "Unsupported edit mode: %s" edit-mode)))
          
          ;; Success - clean up backup
          (delete-file backup-file)
          (json-encode `(("success" . t)
                        ("message" . ,result)
                        ("backup_created" . nil))))
      
      ;; On error, restore backup
      (error
       (when (file-exists-p backup-file)
         (copy-file backup-file file-path t)
         (delete-file backup-file))
       (json-encode `(("success" . nil)
                     ("error" . ,(error-message-string err))
                     ("backup_restored" . t)))))))

;; Register the file editor tool with GPTel
(gptel-make-tool
 :name "file_editor"
 :function #'my/gptel-file-editor
 :description "Comprehensive file editing tool supporting multiple edit modes:
- 'diff': Apply unified diff patches with multi-strategy fallback
- 'search-replace': JSON array of search/replace operations  
- 'lines': Line-based insertions, deletions, and replacements"
 :args '((:name "file_path" 
          :type string
          :description "Path to the file to edit")
         (:name "edit_mode" 
          :type string  
          :description "Edit mode: 'diff', 'search-replace', or 'lines'")
         (:name "edit_data"
          :type string
          :description "Edit instructions based on mode - diff content, JSON operations, or JSON line operations"))
 :category "file-editing")

(register-gptel-tool "file_editor")
(message "file_editor tool loaded")

;https://github.com/lizqwerscott/mcp.el
;https://github.com/karthink/gptel
;https://github.com/github/github-mcp-server
(require 'gptel-integrations)
(require 'mcp-hub)
(setq mcp-server-start-time 120) 

(defun my/get-github-mcp-token ()
  (interactive)
  (when-let ((auth (car (auth-source-search :host "api.github.com" :require '(:secret)))))
    (let ((token (plist-get auth :secret)))
      (if (functionp token) (funcall token) token))))

(defun my/get-brave-token ()
  (interactive)
  (when-let ((auth (car (auth-source-search :host "api.brave.com" :require '(:secret)))))
    (let ((token (plist-get auth :secret)))
      (if (functionp token) (funcall token) token))))

(setq mcp-hub-servers
      `(
       ("filesystem" . (:command "npx"
                                       :args
                                       ("-y" "@modelcontextprotocol/server-filesystem" "/Users/me/nix" "/Users/me/binwarden" "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes")))
        ("brave-search" . (:command "npx"
                                         :args ("-y" "@modelcontextprotocol/server-brave-search")
                                         :env (:BRAVE_API_KEY ,(my/get-brave-token))))
        ("mcp-server-text-editor" . (:command "npx"
                                            :args ("-y" "mcp-server-text-editor")))
        ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
        ("github-mcp" . (:command "docker"
                        :args ("run" "-i" "--rm"
                              "-e" "GITHUB_PERSONAL_ACCESS_TOKEN"
                              "ghcr.io/github/github-mcp-server")
                       :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(my/get-github-mcp-token))))
        ))

;; Start MCP servers after Emacs initializes
(add-hook 'after-init-hook #'mcp-hub-start-all-server)
