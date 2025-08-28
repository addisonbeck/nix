;; Test runner for code-reviews refactored functionality
;; Usage: emacs --batch -l run-tests.el

;; Load required packages
(require 'ert)
(require 'org)
(require 'cl-lib)

;; Load the refactored code (you need to tangle first or load the .el file directly)
(let ((code-file "/Users/me/nix/system/modules/emacs/code-reviews-refactored.el"))
  (when (file-exists-p code-file)
    (load-file code-file)))

;; Load the test file
(let ((test-file "/Users/me/nix/system/modules/emacs/code-reviews-tests.el"))
  (when (file-exists-p test-file)
    (load-file test-file)))

;; Run all tests
(ert-run-tests-batch-and-exit)
