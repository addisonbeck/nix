(require 'org-roam)
(setq org-roam-directory "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/roam")
(org-roam-db-sync)

(defun slugify-org-heading (str)
  "Generate a Pandoc-compatible anchor from org heading string STR."
  (let ((s (downcase str)))
    (setq s (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" s))
    (setq s (replace-regexp-in-string "[[:space:]]+" "-" s))
    (replace-regexp-in-string "-+" "-" s)))

(defun string-demote-org-headings (s)
  "Demote all org headings in string S by one level."
  (replace-regexp-in-string "^\\(\\*+\\)" "*\\1" s))

(defun roam-stitch-recursive-rewrite-links (node-ids visited id-to-title)
  "Stitch NODE-IDS recursively, demoting. Rewrites [[id:...][text]] to [[#slug][text]]."
  (let (result child-ids)
    (dolist (id node-ids)
      (unless (member id visited)
        (let* ((node (org-roam-node-from-id id))
               (title (org-roam-node-title node))
               (file (org-roam-node-file node))
               (id-to-title (cons (cons id title) id-to-title)))
          (when file
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (when (re-search-forward (concat "^\\*+ .*" (regexp-quote title) ".*$") nil t)
                (org-narrow-to-subtree))
              ;; Gather child IDs (as before)
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "\\[\\[id:\\([A-F0-9-]+\\)\\]" nil t)
                  (push (match-string 1) child-ids)))
              ;; Remove PROPERTIES/info drawers
              (goto-char (point-min))
              (while (re-search-forward "^:PROPERTIES:[\0-\377]*?:END:\n?" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward "^:ID: .+\n" nil t)
                (replace-match ""))
              ;; Remove just-link headings
              (goto-char (point-min))
              (while (re-search-forward "^\\*+ +\\(\\[\\[id:[A-F0-9-]+\\]\\[.+?\\]\\]\\)[ \t]*\n?" nil t)
                (replace-match ""))
              ;; Rewrite [[id:...][text]] links to [[#slug][text]]
              (goto-char (point-min))
              (while (re-search-forward "\\[\\[id:\\([A-F0-9-]+\\)\\]\\(\\[\\([^]]+\\)\\]\\)?\\]" nil t)
                (let* ((link-id (match-string 1))
                       (link-title (or (cdr (assoc link-id id-to-title))
                                       ;; fallback: use the text if present, else ID
                                       (match-string 3)
                                       link-id))
                       (slug (slugify-org-heading link-title)))
                  (replace-match (concat "[[#"
                                         slug
                                         "]"
                                         (if (match-string 2) (match-string 2) "")
                                         "]"))))
              (push (buffer-string) result))))))
    (dolist (id (nreverse child-ids))
      (unless (member id visited)
        (let* ((node (org-roam-node-from-id id))
               (title (org-roam-node-title node))
               (file (org-roam-node-file node))
               (id-to-title (cons (cons id title) id-to-title)))
          (when file
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (when (re-search-forward (concat "^\\*+ .*" (regexp-quote title) ".*$") nil t)
                (org-narrow-to-subtree))
              ;; Remove properties etc (same as above)
              (goto-char (point-min))
              (while (re-search-forward "^:PROPERTIES:[\0-\377]*?:END:\n?" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward "^:ID: .+\n" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward "^\\*+ +\\(\\[\\[id:[A-F0-9-]+\\]\\[.+?\\]\\]\\)[ \t]*\n?" nil t)
                (replace-match ""))
              ;; Rewrite links
              (goto-char (point-min))
              (while (re-search-forward "\\[\\[id:\\([A-F0-9-]+\\)\\]\\(\\[\\([^]]+\\)\\]\\)?\\]" nil t)
                (let* ((link-id (match-string 1))
                       (link-title (or (cdr (assoc link-id id-to-title))
                                       (match-string 3)
                                       link-id))
                       (slug (slugify-org-heading link-title)))
                  (replace-match (concat "[[#"
                                         slug
                                         "]"
                                         (if (match-string 2) (match-string 2) "")
                                         "]"))))
              ;; Demote headings for child
              (let ((subtree (string-demote-org-headings (buffer-string))))
                (push subtree result)))))))
    (nreverse result)))

(let* ((start-node-id "9188213C-A1ED-4559-81CF-606695905B2D")  ;; your "Cookbook" node id
       (stitched-org
        (mapconcat #'identity (roam-stitch-recursive-rewrite-links (list start-node-id) nil nil) "\n\n"))
       (output-file "/var/lib/cookbook-to-kindle/cookbook.org"))
  (with-temp-file output-file
    (insert "#+TITLE: Cookbook\n\n")
    (insert stitched-org))
  (princ output-file))
