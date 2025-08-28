# Running Tests for Code Reviews Refactored

## Prerequisites

First, you need to tangle the org file to generate the elisp code:

1. Open `code-reviews-refactored.org` in Emacs
2. Run `M-x org-babel-tangle` (or `C-c C-v t`)
3. This creates `code-reviews-refactored.el`

## Method 1: Interactive Testing in Emacs

```elisp
;; 1. Load the refactored code
(load-file "/Users/me/nix/system/modules/emacs/code-reviews-refactored.el")

;; 2. Load the tests
(load-file "/Users/me/nix/system/modules/emacs/code-reviews-tests.el")

;; 3. Run all tests interactively
M-x ert RET t RET

;; Or run specific test
M-x ert RET test-code-reviews-format-pr-entry RET
```

## Method 2: Batch Mode (Command Line)

```bash
cd /Users/me/nix/system/modules/emacs
emacs --batch -l run-tests.el
```

## Method 3: From Emacs Elisp

```elisp
;; Load everything and run tests
(progn
  (load-file "/Users/me/nix/system/modules/emacs/code-reviews-refactored.el")
  (load-file "/Users/me/nix/system/modules/emacs/code-reviews-tests.el")
  (ert-run-tests-interactively t))
```

## Method 4: Individual Test Functions

```elisp
;; Load the code first
(load-file "/Users/me/nix/system/modules/emacs/code-reviews-refactored.el")
(load-file "/Users/me/nix/system/modules/emacs/code-reviews-tests.el")

;; Run individual tests
(ert-run-test 'test-code-reviews-format-pr-entry)
(ert-run-test 'test-code-reviews-url-cache)
```

## Test Coverage

The test suite includes:

- `test-code-reviews-validate-config` - Configuration validation
- `test-code-reviews-format-pr-entry` - PR entry formatting  
- `test-code-reviews-url-cache` - URL caching functionality
- `test-code-reviews-log` - Logging function
- `test-code-reviews-remove-duplicates-logic` - Duplicate removal logic

## Expected Output

Successful tests will show:
```
Ran 5 tests, 5 results as expected (2024-01-19 14:20:30-0800)
```

Failed tests will show detailed failure information with expected vs actual values.
