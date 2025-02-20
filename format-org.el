(require 'org)
(require 'lisp-mode)

(defun format-org-elisp-blocks ()
  "Format all Emacs Lisp source blocks in the current Org file."
  (org-babel-map-src-blocks nil
    (when (string= lang "emacs-lisp")
      (let ((code (org-element-property :value (org-element-at-point)))
            (point-marker (point-marker)))
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert code)
          (indent-region (point-min) (point-max))
          (setq formatted-code (buffer-string)))
        (goto-char point-marker)
        (org-babel-update-block-body formatted-code)))))

;; Main execution
(let ((file (car command-line-args-left)))
  (find-file file)
  (format-org-elisp-blocks)
  (save-buffer))
