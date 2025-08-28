;; -*- lexical-binding: t; -*-
;; Unit tests for code-reviews refactored functionality

(require 'ert)
(require 'cl-lib)

;; Mock data for testing
(defvar test-pr-data
  '((title . "Fix critical bug in authentication")
    (url . "https://github.com/example/repo/pull/123")
    (repository (nameWithOwner . "example/repo"))
    (author (login . "testuser"))
    (updatedAt . "2024-01-15T10:30:00Z")
    (state . "OPEN")))

;; Test configuration validation
(ert-deftest test-code-reviews-validate-config ()
  "Test configuration validation function."
  (let ((my/github-pr-file nil)
        (my/github-pr-queries nil))
    (should-error (my/code-reviews--validate-config)))
  
  (let ((my/github-pr-file "/nonexistent/path/file.org")
        (my/github-pr-queries '(("test" . "query"))))
    (should-error (my/code-reviews--validate-config))))

;; Test PR entry formatting
(ert-deftest test-code-reviews-format-pr-entry ()
  "Test formatting of PR entries."
  (let ((formatted (my/code-reviews--format-pr-entry test-pr-data)))
    (should (string-match-p "\\* TODO Fix critical bug in authentication" formatted))
    (should (string-match-p ":PR_URL: https://github.com/example/repo/pull/123" formatted))
    (should (string-match-p ":REPO: example/repo" formatted))
    (should (string-match-p ":AUTHOR: testuser" formatted))))

;; Test URL cache functionality
(ert-deftest test-code-reviews-url-cache ()
  "Test URL caching functionality."
  (let ((my/code-reviews--url-cache nil))
    ;; Initially should not exist
    (should-not (my/code-reviews--pr-exists-p "https://test.com/pr/1"))
    
    ;; Add to cache
    (my/code-reviews--add-pr-to-cache "https://test.com/pr/1")
    
    ;; Should now exist
    (should (my/code-reviews--pr-exists-p "https://test.com/pr/1"))
    
    ;; Different URL should not exist
    (should-not (my/code-reviews--pr-exists-p "https://test.com/pr/2"))))

;; Test logging function
(ert-deftest test-code-reviews-log ()
  "Test logging functionality."
  ;; Capture messages by temporarily overriding the message function
  (let ((captured-messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) captured-messages))))
      
      ;; Test basic logging
      (my/code-reviews--log "INFO" "Test message")
      (should (string-match-p "Code Reviews:" (car captured-messages)))
      (should (string-match-p "Test message" (car captured-messages)))
      
      ;; Test formatted logging
      (setq captured-messages '())
      (my/code-reviews--log "INFO" "Test formatted %d" 42)
      (should (string-match-p "Test formatted 42" (car captured-messages))))))

;; Test duplicate removal counter
(ert-deftest test-code-reviews-remove-duplicates-logic ()
  "Test the logic for duplicate removal."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO PR 1
:PROPERTIES:
:PR_URL: https://github.com/test/repo/pull/1
:END:

* TODO PR 2  
:PROPERTIES:
:PR_URL: https://github.com/test/repo/pull/2
:END:

* TODO PR 1 Duplicate
:PROPERTIES:
:PR_URL: https://github.com/test/repo/pull/1
:END:
")
    
    (let ((seen-urls (make-hash-table :test 'equal))
          (removed-count 0))
      (org-map-entries
       (lambda ()
         (let ((pr-url (org-entry-get nil "PR_URL")))
           (if (and pr-url (gethash pr-url seen-urls))
               (cl-incf removed-count)
             (when pr-url
               (puthash pr-url t seen-urls))))))
      
      ;; Should find one duplicate
      (should (= removed-count 1)))))

(provide 'code-reviews-tests)
