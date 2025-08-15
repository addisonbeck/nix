(require 'org)
(require 'subr-x) ; For string-trim
(setq org-confirm-babel-evaluate nil)
(setq debug-on-error t)

;; List of modules to process
(defvar modules '(
                  "auth.org"
                  "projects.org"
                  "package-sources.org"
                  "mode-line.org"
                  "ui.org"
                  "core.org"
                  "theme.org"))

;; Function to safely trim strings (fallback if string-trim not available)
(defun my/safe-trim (str)
  "Safely trim whitespace from STR."
  (if (fboundp 'string-trim)
      (string-trim str)
    (replace-regexp-in-string "\\`[ \t\n\r]+" ""
                              (replace-regexp-in-string "[ \t\n\r]+\\'" "" str))))

;; Debug function to show what files exist
(defun my/debug-files ()
  "Show what files exist in current directory."
  (message "Files in current directory:")
  (dolist (file (directory-files "."))
    (message "  %s" file)))

;; Create the combined init.el
(with-temp-file "init.el"
  ;; Start with lexical-binding on first line
  (insert ";;; init.el --- Combined Emacs Configuration -*- lexical-binding: t -*-\n\n")
  (insert ";; This file is generated from modular org files during Nix build\n")
  (insert ";; Do not edit manually - changes will be overwritten\n\n")
  
  (my/debug-files)
  
  (dolist (module modules)
    (message "Processing module: %s" module)
    (if (file-exists-p module)
        (progn
          (message "Found module file: %s" module)
          ;; Tangle the module (this creates whatever files are specified in tangle headers)
          (condition-case err
              (org-babel-tangle-file module)
            (error (message "Error tangling %s: %s" module err)))
          
          (my/debug-files) ; Show files after tangling
          
          ;; Find all .el files that were created and read their content
          (let ((el-files (directory-files "." nil "\\.el$")))
            (message "Found .el files: %S" el-files)
            (dolist (el-file el-files)
              (unless (or (string= el-file "init.el")          ; Skip our output file
                          (string= el-file "tangle.el")        ; Skip the script file
                          (string= el-file "tangle-script.el") ; Skip this script file
                          (string-prefix-p "debug-" el-file)   ; Skip debug files
                          (string-prefix-p "test-" el-file))   ; Skip test files
                (message "Processing .el file: %s" el-file)
                (let ((content (with-temp-buffer
                                 (insert-file-contents el-file)
                                 (buffer-string))))
                  (message "Content length for %s: %d" el-file (length content))
                  (when (> (length (my/safe-trim content)) 0)
                    (goto-char (point-max))
                    (insert (format "\n;; === From %s (%s) ===\n" module el-file))
                    (insert content)
                    (insert "\n")
                    (message "Added content from %s to init.el" el-file))
                  ;; Clean up the individual tangled file
                  (condition-case err
                      (delete-file el-file)
                    (error (message "Warning: Could not delete %s: %s" el-file err))))))))
      (message "Warning: Module file %s does not exist!" module)))
  
  ;; Add a final newline and some basic content to ensure file is not empty
  (goto-char (point-max))
  (insert "\n;; End of generated configuration\n")
  (insert "(message \"Modular Emacs configuration loaded successfully\")\n"))

(message "Tangling complete!")

;; Debug: Show the final init.el content
(when (file-exists-p "init.el")
  (message "Final init.el size: %d bytes" (nth 7 (file-attributes "init.el")))
  (with-temp-buffer
    (insert-file-contents "init.el")
    (message "First 500 characters of init.el:")
    (message "%s" (substring (buffer-string) 0 (min 500 (length (buffer-string)))))))
