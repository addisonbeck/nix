;;; cookbook-stitch-debug.el --- Org-roam Cookbook to EPUB structurer (debug version)

(require 'org-roam)
(require 'cl-lib)
(setq org-roam-directory "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/roam")
(org-roam-db-sync)

(defvar cookbook--logfile "/var/lib/cookbook-to-kindle/cookbook-stitch.log")

(defun cookbook--log (format &rest args)
  "Log debugging info to file and echo. FORMAT and ARGS as for `message`."
  (let ((msg (apply #'format format args)))
    (with-temp-buffer
      (when (file-exists-p cookbook--logfile)
        (insert-file-contents cookbook--logfile))
      (goto-char (point-max))
      (insert (format-time-string "[%F %T] "))
      (insert msg "\n")
      (write-region (point-min) (point-max) cookbook--logfile))
    (message "%s" msg)))

(defun cookbook--slugify (str)
  "Pandoc-compatible anchor from org heading string STR."
  (let ((s (downcase str)))
    (setq s (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" s))
    (setq s (replace-regexp-in-string "[[:space:]]+" "-" s))
    (replace-regexp-in-string "-+" "-" s)))

(defun cookbook--strip-properties (s)
  "Remove all property drawers and ID fields from S."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward "^:PROPERTIES:[\\0-\\377]*?:END:\n?" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "^:ID: .+\n" nil t)
      (replace-match ""))
    (buffer-string)))

(defun cookbook--replace-id-links (s id-table)
  "Rewrite [[id:...][text]] to [[#slug][text]] in S using ID-TABLE."
  (replace-regexp-in-string
   "\\[\\[id:\\([A-F0-9-]+\\)\\]\\(\\[\\([^]]+\\)\\]\\)?\\]"
   (lambda (m)
     (let* ((id (match-string 1 m))
            (txt (or (and (match-string 3 m) (match-string 3 m)) id))
            (slug (cookbook--slugify (or (cdr (assoc id id-table)) txt))))
       (cookbook--log "Replacing link: id=%s, txt=%s, slug=%s" id txt slug)
       (concat "[[#"
               slug
               "]"
               (if (string= txt id) "" (concat "[" txt "]"))
               "]")))
   s))

(defun cookbook--read-and-slug-table (id)
  "Get cons list of (ID . Title) for all traversed child nodes."
  (let ((seen '()))
    (cl-labels
        ((scan (id)
               (unless (assoc id seen)
                 (let* ((node (ignore-errors (org-roam-node-from-id id))))
                   (if node
                       (let* ((title (org-roam-node-title node))
                              (file (org-roam-node-file node)))
                         (cookbook--log "Indexing node: ID=%s Title=%s File=%s" id title file)
                         (push (cons id title) seen)
                         (when (and file (file-exists-p file))
                           (with-temp-buffer
                             (insert-file-contents file)
                             (goto-char (point-min))
                             ;; collect further child ids
                             (while (re-search-forward "\\[\\[id:\\([A-F0-9-]+\\)\\]" nil t)
                               (scan (match-string 1)))))))
                     (cookbook--log "WARNING: Failed to find node for ID=%s" id)))))
      (scan id))
    seen))

(defun cookbook--expand-node-content (file id base-level id-table)
  "Expand and demote node content (FILE, ID, at BASE-LEVEL with ID-TABLE)."
  (cookbook--log "Expanding node: file=%s id=%s base-level=%d" file id base-level)
  (with-temp-buffer
    (condition-case err
        (progn
          (insert-file-contents file)
          (goto-char (point-min))
          (let* ((title (org-roam-node-title (org-roam-node-from-id id)))
                 (found (re-search-forward (concat "^\\*+.*" (regexp-quote title) ".*$") nil t)))
            (if found
                (progn
                  (org-narrow-to-subtree)
                  (let ((s (buffer-string)))
                    (cookbook--log "Got %d chars for %s/%s" (length s) id file)
                    ;; strip properties
                    (setq s (cookbook--strip-properties s))
                    ;; replace id-links
                    (setq s (cookbook--replace-id-links s id-table))
                    ;; demote headings
                    (setq s
                          (replace-regexp-in-string
                           "^\\(\\*+\\)"
                           (lambda (m)
                             (let ((n (+ (length m) (max 0 (- base-level 1)))))
                               (make-string n ?*)))
                           s))
                    s))
              (cookbook--log "ERROR: Subtree not found in %s for ID=%s, Title=%s" file id title)
              "")))
      (error
       (cookbook--log "ERROR (expand-node): %S" err)
       ""))))

(defun cookbook--expand-cookbook-outline (cookbook-id)
  "Expand the Cookbook org node and inline all [[id:...]] links."
  (cookbook--log "==== BEGIN cookbook--expand-cookbook-outline ====")
  (let* ((cookbook-node (org-roam-node-from-id cookbook-id))
         (file (org-roam-node-file cookbook-node))
         (id-table (cookbook--read-and-slug-table cookbook-id))
         (outline-lines '()))
    (cookbook--log "Cookbook root: ID=%s, File=%s" cookbook-id file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*+.*" (regexp-quote (org-roam-node-title cookbook-node)) ".*$") nil t)
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (cond
             ;; Org Headings: output as is
             ((string-match-p "^\\*+" line)
              (push line outline-lines)
              (cookbook--log "Heading: %s" line))
             ;; Org-roam ID links: expand
             ((string-match "\\[\\[id:\\([A-F0-9-]+\\)\\]" line)
              (let* ((link-id (match-string 1 line))
                     (child-node (ignore-errors (org-roam-node-from-id link-id)))
                     (child-file (and child-node (org-roam-node-file child-node)))
                     (base-level (save-excursion
                                   (beginning-of-line)
                                   (when (re-search-backward "^\\*+" nil t)
                                     (+ 1 (length (match-string 0))))
                                   2)))  ; fallback = 2
                (cookbook--log "Found id-link: %s -> file=%s" link-id child-file)
                (if (and child-file (file-exists-p child-file))
                    (push (cookbook--expand-node-content child-file link-id base-level id-table) outline-lines)
                  (cookbook--log "WARNING: Could not expand ID=%s at line: %s" link-id line))))
             ;; Otherwise, copy as is
             (t
              (push line outline-lines)))
            (forward-line 1))))
    (cookbook--log "==== END cookbook--expand-cookbook-outline ====")
    (mapconcat #'identity (nreverse outline-lines) "\n"))))

;; Entrypoint: Run and write output
(let* ((cookbook-id "9188213C-A1ED-4559-81CF-606695905B2D")
       (output-file "/var/lib/cookbook-to-kindle/cookbook.org")
       (stitched (cookbook--expand-cookbook-outline cookbook-id)))
  (cookbook--log "Writing output to: %s" output-file)
  (with-temp-file output-file
    (insert "#+TITLE: Cookbook\n\n")
    (insert stitched))
  (cookbook--log "DONE. Output file: %s" output-file)
  (princ output-file))

;;; cookbook-stitch-debug.el ends here
