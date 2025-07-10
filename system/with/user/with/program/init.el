;; init.el --- My personal Emacs configuration  -*- lexical-binding: t -*-

;; Commentary:
;; This is my personal Emacs configuration file.
;; It sets up various packages and configurations for development work.

;; Code:

(setq auth-sources '("~/.authinfo"))

(defvar my/projects
  '(("nix"       . (:path "~/nix"))
    ("notes" . (:path "~/notes"))
    ("clients" . (:path "~/bitwarden/clients"))
    ("server" . (:path "~/bitwarden/server"))
    ("sdk-internal" . (:path "~/bitwarden/sdk-internal"))
    ("nx-poc"    . (:path "~/bitwarden/bitwarden-nx-poc"))
    ("wg-open-source" . (:path "~/bitwarden/wg-open-source-at-bitwarden"))
    ("binwarden" . (:path "~/binwarden"))
    ("d" . (:path "~/d"))
    ("contributing-docs" . (:path "~/bitwarden/contributing-docs")))
  "Alist of projects with metadata")

(defun my/get-project-attr (project-name attr)
  "Get ATTR for PROJECT-NAME."
  (plist-get (alist-get project-name my/projects nil nil #'string=) attr))

(defun my/get-project-path (project-name)
  "Get the path for PROJECT-NAME."
  (my/get-project-attr project-name :path))

(defun my/find-project (project-name)
  "Open a project's root directory."
  (interactive
   (list (completing-read "Project: " (mapcar #'car my/projects))))
  (find-file (my/get-project-path project-name)))

(defun my/project-dired ()
  "Open dired in a project directory."
  (interactive)
  (let ((project-name (completing-read "Project: " (mapcar #'car my/projects))))
    (dired (my/get-project-path project-name))))

(defun my/compile-in-project (project-name command)
  "Run compilation COMMAND in PROJECT-NAME."
  (interactive
   (let* ((project (completing-read "Project: " (mapcar #'car my/projects)))
	    (command (read-string "Command: " nil 'compile-history)))
     (list project command)))
  (let* ((default-directory (my/get-project-path project-name))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*compile-%s*" project-name))))
    (compile command t)))

(defun my/terminal-in-project ()
  "Open terminal in a project directory."
  (interactive)
  (let* ((project-name (completing-read "Project: " (mapcar #'car my/projects)))
	   (default-directory (my/get-project-path project-name)))
    (vterm (format "*vterm-%s*" project-name))))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("org" . "https://orgmode.org/elpa/")
			   ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Configure frame appearance
(setq default-frame-alist
	'((menu-bar-lines . 0)
	  (tool-bar-lines . 0)
	  (vertical-scroll-bars)
	  (left-fringe . 0)
	  (right-fringe . 0)
	  (internal-border-width . 0)
	  (undecorated . t)
	  (fullscreen . maximized)))

(setq inhibit-startup-message t)
(setq initial-frame-alist default-frame-alist)

(advice-add #'display-startup-echo-area-message :override #'ignore)

(defun my/current-project-name ()
  "Get the name of the current project from my/projects if there is one."
  (when-let* ((file-path (buffer-file-name))
		(abs-path (expand-file-name file-path)))
    (catch 'found
	(dolist (project my/projects)
	  (let* ((project-name (car project))
		 (project-path (expand-file-name (my/get-project-path project-name))))
	    (when (string-prefix-p project-path abs-path)
	      (throw 'found project-name))))
	nil)))

(setq-default mode-line-format
		(list
		 ;; Current project (if any)
		 '(:eval (when-let ((project (my/current-project-name)))
			   (propertize (format "%s/" project) 'face 'mode-line-emphasis)))
		 ;; Filename
		 '(:eval (propertize "%b " 'face 'mode-line-buffer-id))
		 ;; Major mode
		 '(:eval (propertize (format " %s " major-mode) 'face 'mode-line-buffer-id))
		 ;; Git branch and status using vc-mode
		 '(:eval (when vc-mode
			   (let ((branch (replace-regexp-in-string "^ Git[:-]" "" vc-mode)))
			     (concat
			      (propertize " " 'face 'buffer-file-name)
			      (propertize (format "%s" branch) 'face 'mode-line-emphasis)
			      (propertize (if (vc-state buffer-file-name) " ++" "") 'face
					  (if (vc-state buffer-file-name) 'error 'success))
			      (propertize " " 'face 'buffer-file-name)))))
		 ))

;; Disable backup files and configure indentation
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(electric-indent-mode 1)
(setq-default tab-width 2
		indent-tabs-mode nil
		tab-stop-list (number-sequence 2 120 2))
(setq confirm-kill-emacs nil)
(setq auto-save-default nil
	create-lockfiles nil)
(setq select-enable-clipboard t)
(setq case-fold-search t)
(xterm-mouse-mode 1)
(setq-default fill-column 77)
(global-hl-line-mode -1)
(setq sentence-end-double-space nil)

(defun my/extract-quotes-from-org-files ()
  "Extract headlines tagged with :quote: from org files in notes directory."
  (let ((quotes '())
        (notes-dir "~/notes"))
    (dolist (file (directory-files-recursively notes-dir "\\.org$"))
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+\\s-+\\(.*?\\)\\s-+:quote:" nil t)
          (let* ((headline (match-string-no-properties 1))
                 (element (org-element-at-point))
                 (content (org-element-property :contents-begin element))
                 (end (org-element-property :contents-end element))
                 (raw-text (when (and content end)
                             (string-trim (buffer-substring-no-properties content end))))
                 (quote-text
                  (when raw-text
                    ;; Process text to handle quote blocks
                    (with-temp-buffer
                      (insert raw-text)
                      ;; Replace #+begin_quote and #+end_quote with empty strings
                      (goto-char (point-min))
                      (while (re-search-forward "^[ \t]*#\\+begin_quote[ \t]*$" nil t)
                        (replace-match ""))
                      (goto-char (point-min))
                      (while (re-search-forward "^[ \t]*#\\+end_quote[ \t]*$" nil t)
                        (replace-match ""))
                      ;; Return the cleaned text
                      (string-trim (buffer-string))))))
            (when (and headline (not (string-empty-p headline))
                       quote-text (not (string-empty-p quote-text)))
              (push (format "%s\n\n— %s" quote-text headline) quotes))))))
    (or quotes
        ;; Fallback quotes if none found
        '("Emacs is the extensible self-documenting editor.\n\n— GNU Emacs"
          "The journey of a thousand miles begins with one step.\n\n— Lao Tzu"))))

  (use-package dashboard
    :ensure t
    :init
    (setq dashboard-icon-type 'nerd-icons)
    (setq dashboard-projects-backend 'projectile)
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-center-content t
	  dashboard-items '((recents . 5)
			    (projects . 5)
			    (bookmarks . 5))
	  dashboard-set-heading-icons t
	  dashboard-set-file-icons t
	  dashboard-show-shortcuts t
          dashboard-set-footer t
          dashboard-footer-messages (my/extract-quotes-from-org-files))
    (setq dashboard-heading-icons '((recents   . "nf-oct-history")
				    (bookmarks . "nf-oct-bookmark")
				    (projects  . "nf-oct-project"))))

;; Test
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  ;; Custom movement functions
  (defun evil-move-half-page-down ()
    "Move cursor half page down"
    (interactive)
    (evil-next-line (/ (window-height) 4))
    (evil-scroll-line-to-center nil))

  (defun evil-move-half-page-up ()
    "Move cursor half page up"
    (interactive)
    (evil-previous-line (/ (window-height) 4))
    (evil-scroll-line-to-center nil))

  ;; Bind J and K to half-page movement
  (define-key evil-normal-state-map (kbd "J") 'evil-move-half-page-down)
  (define-key evil-normal-state-map (kbd "K") 'evil-move-half-page-up))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Configure evil-collection for magit
(with-eval-after-load 'evil-collection-magit
  (evil-collection-define-key 'normal 'magit-status-mode-map
				"V" #'magit-start-region-select))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
(evil-define-key 'motion org-agenda-mode-map
		   (kbd "C-b") 'projectile-switch-to-buffer
		   (kbd "C-p") 'projectile-switch-project
		   (kbd "C-f") 'projectile-find-file
		   (kbd "<left>") 'org-agenda-earlier
		   (kbd "<right>") 'org-agenda-later
		   (kbd "C-c j") 'org-agenda-goto-date
		   (kbd "gx")  'org-agenda-open-link
		   (kbd "t") 'org-agenda-todo
		   (kbd "T") 'org-agenda-todo-yesterday)

(defun cycle-line-numbers ()
  "Cycle through line number modes: off -> relative -> normal -> off."
  (interactive)
  (cond
   ;; If currently off, switch to relative
   ((not display-line-numbers)
    (setq display-line-numbers 'relative)
    (message "Line numbers: RELATIVE"))

   ;; If currently relative, switch to normal
   ((eq display-line-numbers 'relative)
    (setq display-line-numbers t)
    (message "Line numbers: NORMAL"))

   ;; If currently normal, switch to off
   (t
    (setq display-line-numbers nil)
    (message "Line numbers: OFF"))))

;; Bind to "N" in evil normal mode
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global "N" 'cycle-line-numbers))

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-command-map (kbd "d") 'projectile-find-file-in-directory)
  (define-key projectile-command-map (kbd "P") 'my/projectile-find-file-in-all-projects)
  (setq projectile-indexing-method 'alien)
  (setq projectile-git-command "git ls-files -zco -X ~/.gitignore")
  (setq projectile-known-projects
	  (mapcar (lambda (project)
		    (expand-file-name (my/get-project-path (car project))))
		  my/projects))
  (setq projectile-auto-discover nil)
  (projectile-save-known-projects)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(require 'consult)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Custom find-from-here function
(defun find-from-here ()
  "Find files from current buffer's directory."
  (interactive)
  (when buffer-file-name
    (consult-find (file-name-directory buffer-file-name))))

(global-set-key (kbd "C-c d") 'find-from-here)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt)
  (setq treesit-auto-langs '(typescript javascript tsx jsx yaml)))

(use-package typescript-ts-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-ts-mode)
	   ("\\.tsx\\'" . tsx-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  (add-to-list 'major-mode-remap-alist '(tsx-mode . tsx-ts-mode)))

;; Ensure typescript grammar is installed
(unless (treesit-language-available-p 'typescript)
  (treesit-install-language-grammar 'typescript))

(use-package lsp-mode
  :ensure t
  :hook ((typescript-ts-mode . lsp)
	   (tsx-ts-mode . lsp)
	   (typescript-mode . lsp)
	   (csharp-mode . lsp)
	   (rust-mode . lsp)
	   (nix-mode . lsp)
	   (json-mode . lsp)
	   (sql-mode . lsp)
	   (lua-mode . lsp))
  :commands lsp
  :config
  ;;(setq lsp-nix-nixd-server-path "nixd")
  ;;(setq lsp-enabled-clients '(nixd-lsp))
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (add-to-list 'lsp-disabled-clients 'copilot-ls)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\chats\\'")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-mode nil)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode for completions
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1
	  company-idle-delay 0.0))

;; Add flycheck configuration
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(defun magit-status-project ()
  "Switch project and open magit."
  (interactive)
  (let ((projectile-switch-project-action 'magit-status))
    (projectile-switch-project)))

(global-set-key (kbd "C-c m") 'magit-status-project)

(use-package forge
  :ensure t
  :after magit
  :config
  ;; Configure GitHub authentication
  (setq auth-sources '("~/.authinfo"))
  ;; Optionally set the number of items to fetch
  (setq forge-topic-list-limit '(60 . 0)))

;; Basic settings
(setq notes-directory "~/notes")
(setq markdown-command "pandoc")

;; Markdown configuration
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
	   ("\\.markdown\\'" . markdown-mode)))

(require 'transient)

(defcustom my-notes-directory "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/"
  "Path to my notes directory."
  :type 'directory
  :group 'org)
(defcustom my-nix-systems-flake-directory "/Users/me/nix"
  "Path to my nix directory."
  :type 'directory
  :group 'org)
(setq org-directory my-notes-directory)

(setq org-startup-truncated nil)

(setq org-log-done 'time)

(setq org-log-into-drawer t)

(setq org-startup-folded 'fold)

(setq org-auto-align-tags nil)

(setq org-export-backends '(html icalendar latex man md org json))

(setq org-image-max-width 120)

(setq org-startup-with-inline-images t)

(setq org-cycle-inline-images-display t)

(setq org-display-remote-inline-images 'download)

(setq org-return-follows-link t)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)

;; Define your custom face settings in a function
(defun my/apply-custom-faces ()
  "Apply all my custom face settings."
  ;; Basic font settings
  (set-face-attribute 'default nil :family "Iosevka" :height 140)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 140)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 140)
  
  ;; Org faces
  (with-eval-after-load 'org
    (custom-set-faces
     '(org-document-info-keyword ((t (:inherit variable-pitch :height 1.0))))
     '(org-document-title ((t (:inherit variable-pitch :height 1.3))))
     '(org-level-1 ((t (:inherit variable-pitch :height 1.3))))
     '(org-level-2 ((t (:inherit variable-pitch :height 1.2))))
     '(org-level-3 ((t (:inherit variable-pitch :height 1.1))))
     '(org-level-4 ((t (:inherit variable-pitch :height 1.0))))
     '(org-level-5 ((t (:inherit variable-pitch :height 1.0))))
     '(org-level-6 ((t (:inherit variable-pitch :height 1.0))))
     '(org-level-7 ((t (:inherit variable-pitch :height 1.0))))
     '(org-level-8 ((t (:inherit variable-pitch :height 1.0))))
     '(org-agenda-date-today ((t (:inherit variable-pitch :height 1.3))))
     '(org-super-agenda-header ((t (:inherit variable-pitch :height 1.2))))
     
     ;; Keep these elements as fixed-pitch even in variable-pitch-mode
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit fixed-pitch))))
     '(org-table ((t (:inherit fixed-pitch))))
     '(org-verbatim ((t (:inherit fixed-pitch))))
     '(org-special-keyword ((t (:inherit fixed-pitch :height 140))))
     '(org-drawer ((t (:inherit fixed-pitch :height 140))))
     '(org-property-value ((t (:inherit fixed-pitch :height 140))))
     '(org-modern-label ((t (:inherit fixed-pitch :height 140))))
     '(org-modern-statistics ((t (:inherit fixed-pitch :height 140))))
     '(org-modern-tag ((t (:inherit fixed-pitch :height 140)))))))

;; Apply custom faces when Emacs starts
(add-hook 'after-init-hook 'my/apply-custom-faces)

(setq org-modern-hide-stars nil)
(setq org-modern-star nil)

(add-hook 'markdown-mode-hook (lambda ()
  (breadcrumb-local-mode 1)
  (variable-pitch-mode 1)
  (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)))

(add-hook 'prog-mode (lambda ()
  (breadcrumb-local-mode 1)
  (variable-pitch-mode 1)
  (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp :tangle no . t)
   (shell . t)
   (org . t)
   (mermaid . t)
   ))

(setq org-babel-sh-command "bash -l -c")

(setenv "PUPPETEER_EXECUTABLE_PATH" 
  (or (executable-find "google-chrome-stable")
      (executable-find "google-chrome")))

(setq org-src-preserve-indentation nil
org-edit-src-content-indentation 0)

(require 'ox-json)

(use-package ob-mermaid
  :config
  ;; Set the path to the mermaid CLI using the custom puppeteer-cli
  (setq ob-mermaid-cli-path (executable-find "mmdc"))
  ;; If you need to specify the Chrome executable directly:
  (setq ob-mermaid-browser-path (executable-find "google-chrome-stable")))

;; Enable automatic display of inline images after executing babel blocks
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(setq org-redisplay-inline-images t)

;; Allow evaluation of code blocks without confirmation for safe languages
(defun my/org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp :tangle no" "shell"))))
;;(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)
(setq org-confirm-babel-evaluate nil)

;; Ensure pretty fontification of source blocks
(setq org-src-fontify-natively t)

;;(require 'ob-async) ;; Allow for asyncround running of babel blocks

;; Custom keybinding for executing all source blocks in a subtree
(define-key org-mode-map (kbd "C-c C-v C-t") 'org-babel-execute-subtree)

(require 'org-make-toc)

(define-key org-mode-map (kbd "RET") 'newline)

;; These bindings just emulate the defaults instead of doing a bunch of weird org specific stuff.
(evil-define-key 'insert org-mode-map (kbd "RET") 'newline)
(evil-define-key 'insert org-mode-map (kbd "TAB") 'tab-to-tab-stop)
(defun my-org-evil-open-below ()
  "Open line below preserving org structure but preventing reformatting."
  (interactive)
  ;; Use evil's basic open behavior
  (evil-open-below 1)
  ;; Exit insert state then re-enter to avoid auto-formatting
  (evil-normal-state)
  (evil-insert-state))

(evil-define-key 'normal org-mode-map "o" 'my-org-evil-open-below)

;; Org agenda files cache system for better performance
(defvar my/org-agenda-files-cache nil
  "Cached list of org agenda files.")

(defvar my/org-agenda-files-cache-time 0
  "Time when the org agenda files cache was last updated.")

(defvar my/org-agenda-files-cache-duration (* 60 60)
  "How long to use cached org agenda files before refreshing (in seconds).")

(defun my/update-org-agenda-files-cache ()
  "Update the cached list of org agenda files."
  (interactive)
  (message "Refreshing org agenda files cache...")
  (setq my/org-agenda-files-cache
        (seq-filter (lambda (f) (not (string-match-p "/\\." f)))
                    (directory-files-recursively "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/" "\\.org$")))
  (setq my/org-agenda-files-cache-time (float-time (current-time)))
  (message "Org agenda files cache updated (%d files)" (length my/org-agenda-files-cache)))

(defun my/get-org-agenda-files ()
  "Get org agenda files, using cache if it's not expired."
  (when (or (not my/org-agenda-files-cache)
            (> (- (float-time (current-time)) my/org-agenda-files-cache-time)
               my/org-agenda-files-cache-duration))
    (my/update-org-agenda-files-cache))
  my/org-agenda-files-cache)

(defun my/set-org-agenda-files ()
  "Set org-agenda-files using the cached file list."
  (interactive)
  (setq org-agenda-files (my/get-org-agenda-files)))

;; Initialize the cache at startup
(add-hook 'after-init-hook 'my/update-org-agenda-files-cache)

;; Update the agenda files when entering org-agenda
(add-hook 'org-agenda-mode-hook 'my/set-org-agenda-files)

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

(setq org-directory "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/")
(setq org-default-notes-file "inbox.org")
(setq org-capture-templates
'(
  ("l" "Log" entry
   (file org-default-notes-file)
   "* %U \n%?")
  ("e" "Event" entry
   (file org-default-notes-file)
   "* %^{Title}\n%^T\n%?")
  ("t" "Todo" entry
   (file org-default-notes-file)
   "* TODO %^{Title}\nDEADLINE: %t\n%?")
  ("a" "A link to the current location in the current file" entry
   (file org-default-notes-file)
   "* %a")
  ))

(defgroup my/budget nil
  "My budget capture settings."
  :group 'org-capture)

(defcustom my/budget-file "~/notes/budget.org"
  "Org file containing my budget."
  :type 'file
  :group 'my/budget)

(defcustom my/budget-payees-heading "Payees"
  "Heading name under which payees are listed in the org file."
  :type 'string
  :group 'my/budget)

(defcustom my/budget-accounts-heading "Accounts"
  "Heading name under which accounts are listed in the org file."
  :type 'string
  :group 'my/budget)

(defcustom my/budget-categories-heading "Categories"
  "Heading name under which categories are listed in the org file."
  :type 'string
  :group 'my/budget)

(defun my/load-budget-items-from-org (heading-name)
  "Load items from subheadings under the specified HEADING-NAME."
  (when (file-exists-p my/budget-file)
    (with-temp-buffer
      (insert-file-contents my/budget-file)
      (org-mode)
      (let (items)
        ;; Find the specified heading
        (goto-char (point-min))
        (when (re-search-forward (format "^\\*+\\s-+%s" (regexp-quote heading-name)) nil t)
          (let ((parent-level (org-outline-level)))
            (org-map-entries
             (lambda ()
               (when (= (org-outline-level) (1+ parent-level))
                 ;; This is a direct child of our heading
                 (push (org-get-heading t t t t) items)))
             nil 'tree)))
        (nreverse items)))))

(defun my/load-budget-payees-from-org ()
  "Load payees from subheadings under the Payees heading."
  (my/load-budget-items-from-org my/budget-payees-heading))

(defun my/load-budget-accounts-from-org ()
  "Load accounts from subheadings under the Accounts heading."
  (my/load-budget-items-from-org my/budget-accounts-heading))

(defun my/load-budget-categories-from-org ()
  "Load categories from subheadings under the Categories heading."
  (my/load-budget-items-from-org my/budget-categories-heading))

(defcustom my/budget-payees (my/load-budget-payees-from-org)
  "List of payees for budget capture."
  :type '(repeat string)
  :group 'my/budget)

(defcustom my/budget-accounts (my/load-budget-accounts-from-org)
  "List of accounts for budget capture."
  :type '(repeat string)
  :group 'my/budget)

(defcustom my/budget-categories (my/load-budget-categories-from-org)
  "List of categories for budget capture."
  :type '(repeat string)
  :group 'my/budget)

(defun my/budget--capture-template ()
  "Return an org-capture template string for a budget transaction."
  ;; Refresh lists from org file
  (setq my/budget-payees (my/load-budget-payees-from-org))
  (setq my/budget-accounts (my/load-budget-accounts-from-org))
  (setq my/budget-categories (my/load-budget-categories-from-org))

  (let* ((date     (org-read-date nil nil nil "Date: "))
         (amount   (read-string "Amount: "))
         (payee    (completing-read "Payee: " my/budget-payees))
         (account  (completing-read "Account: " my/budget-accounts))
         (category (completing-read "Category: " my/budget-categories)))
    (concat
     "** [PENDING] [" date "]: $" amount " @ " payee " on " account "\n"
     "%^{Description}\n"
     "#+begin_src ledger\n"
     date " ! " payee "\n"
     "    " category "  $" amount "\n"
     "    " account "\n"
     "#+end_src\n")))

(add-to-list 'org-capture-templates `("b" "Budgeting templates"))

(add-to-list 'org-capture-templates
             `("bt" "Budget transaction" entry
               (file "~/notes/inbox.org")
               (function my/budget--capture-template)
               :empty-lines 1))

(defun my/budget--capture-new-payee ()
  "Return an org-capture template string for a new budget payee."
  (let* ((payee (read-string "New Payee Name: ")))
    (format "* %s%%?" payee)))

(add-to-list 'org-capture-templates
             `("bp" "Budget payee" entry
               (file+headline ,my/budget-file ,my/budget-payees-heading)
               (function my/budget--capture-new-payee)
               :empty-lines 0))

(defun my/budget--capture-new-account ()
  "Return an org-capture template string for a new budget account."
  (let* ((account (read-string "New Account Name: ")))
    (format "* %s%%?" account)))

(add-to-list 'org-capture-templates
             `("ba" "Budget account" entry
               (file+headline ,my/budget-file ,my/budget-accounts-heading)
               (function my/budget--capture-new-account)
               :empty-lines 0))

(defun my/budget--capture-new-category ()
  "Return an org-capture template string for a new budget category."
  (let* ((category (read-string "New Category Name: ")))
    (format "* %s\n%%?" category)))

(add-to-list 'org-capture-templates
             `("bc" "Budget category" entry
               (file+headline ,my/budget-file ,my/budget-categories-heading)
               (function my/budget--capture-new-category)
               :empty-lines 0))

(defun convert-to-org ()
  "Convert current markdown buffer to org format."
  (interactive)
  (let* ((md-file (buffer-file-name))
	   (org-file (concat (file-name-sans-extension md-file) ".org")))
    (when (and md-file (file-exists-p md-file))
	(call-process "pandoc" nil nil nil
		      "-f" "markdown"
		      "-t" "org"
		      md-file
		      "-o" org-file)
	(find-file org-file))))

(defun convert-to-markdown ()
  "Convert current org buffer to markdown format."
  (interactive)
  (let* ((org-file (buffer-file-name))
	   (md-file (concat (file-name-sans-extension org-file) ".md")))
    (when (and org-file (file-exists-p org-file))
	(call-process "pandoc" nil nil nil
		      "-f" "org"
		      "-t" "markdown"
		      org-file
		      "-o" md-file)
	(find-file md-file))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-o") 'convert-to-org))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-m") 'convert-to-markdown))

(defun my/move-to-custom-id-file ()
  "Move selected org item to a new file named after its CUSTOM_ID property."
  (interactive)
  (save-excursion
    (let* ((region-content (buffer-substring (region-beginning) (region-end)))
	     (custom-id (save-excursion
			  (goto-char (region-beginning))
			  (org-entry-get nil "CUSTOM_ID"))))
	(if custom-id
	    (let ((new-file (concat "~/notes/" custom-id ".org")))
	      (with-temp-file new-file
		(insert "#+TITLE: " custom-id "\n\n")
		(insert region-content))
	      (delete-region (region-beginning) (region-end))
	      (insert (format "[[file:%s][%s]]\n" new-file custom-id))
	      (message "Moved to %s" new-file))
	  (message "No CUSTOM_ID property found!")))))

(setq org-agenda-block-separator nil)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-timegrid-use-ampm t)
(setq org-agenda-time-leading-zero t)
(setq org-agenda-todo-keyword-format "%s")
(setq org-agenda-include-diary t)
(setq org-agenda-hide-tags-regexp ".")

;; Allow creating new nodes (including new files) when refiling
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Use the full outline paths for refile targets
(setq org-refile-use-outline-path nil)

;; Completes in steps so you can select a heading after selecting the file
(setq org-outline-path-complete-in-steps nil)

(require 'diary-lib)

(use-package org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-header-map nil)  
  (setq org-super-agenda-header-properties nil)
  (org-super-agenda-mode))

(setq warning-suppress-types '((org-element)))

(defun my/inherit-meeting-times ()
  "Set SCHEDULED property on meeting note TODOs based on parent timestamp."
  (interactive)
  (org-map-entries
   (lambda ()
     (when (and (string= (org-entry-get nil "CATEGORY") "meeting-notes")
	  (org-get-todo-state))
 (let ((timestamp nil))
   (save-excursion
     (when (org-up-heading-safe)
       (setq timestamp (org-entry-get nil "TIMESTAMP" t))))
   (when timestamp
     (org-schedule nil timestamp)))))
   "+CATEGORY=\"meeting-notes\"+TODO=\"TODO\""))

(setq org-agenda-custom-commands
'(("d" "daily dashboard"
   (
    (tags "+CATEGORY=\"inbox\"" 
	  ((org-agenda-overriding-header "Inbox")))
    (agenda "Schedule and Habits"
	    ((org-agenda-span 'day)
	     (org-agenda-sorting-strategy '((agenda time-up todo-state-down alpha-up)))
	     (org-agenda-overriding-header " ")
	     (org-super-agenda-groups
	      '(
		(:name "Happening today" 
		       :and(:scheduled nil :deadline nil :not(:time-grid t)))
		(:name "Today's Meeting Notes" :category "meeting-notes")
		(:name "Today's Schedule"
		       :time-grid t)
		(:name "High Priority" :and (:priority "A" :deadline today))
		(:name "Overdue" :deadline past)
	  (:name "Cooking" :and (:deadline today :tag "cooking"))
	  (:name "Hard" :and (:deadline today :tag "hard"))
		(:name "Quick" :and (:deadline today :tag "quick"))
		(:name "Easy" :and (:deadline today :tag "easy"))
	  (:name "Cleaning" :and (:deadline today :tag "cleaning"))
		(:name "Code Review" :and (:category "code review" :deadline today))
		(:name "Code Review Bunker" :and (:category "code review bunker" :deadline today))
		(:name "Work" :and (:deadline today :tag "work"))
		(:name "Due Today" :deadline today)
		;(:name "High Priority" :priority "A")
		;(:name "Easy" :tag ("easy"))
		(:name "Due Soon" :deadline future)
		(:name "Active Bugs" :category "bug")
		(:name "Active Epics" :category "epic")
		;; I moved TODOs to a tags component because agenda won't show none todo/event items like logs
		;;(:name "Inbox" :category "inbox")
		;;(:name "Poetry" :category "my poems")
		(:name "Family Stuff" :category "family")
		(:name "Holidays" :category "holiday")
		(:name "The Garden" :category "the-garden")
		(:name "Logs" :category "log")
		(:name "Re: Me" :category "me")
		(:name "Re: Emily" :category "emily")
		(:name "Re: Lincoln" :category "lincoln")
		(:name "Re: Nora" :category "nora")
		(:name "Re: Fern" :category "fern")
		(:name "Re: Harry" :category "harry")
		(:name "AM Habits" :category "personal habits am")
		(:name "Midday Habits" :category "personal habits midday")
		(:name "PM Habits" :category "personal habits pm")
		(:name "Any Time Habits" :category "personal habits any time")
		(:name "Work Habits" :category "work habits")
		(:auto-category t)
		))))))
  ("w" "Weekly overview with super-agenda"
   ((agenda ""
	    ((org-agenda-span 7)                      ;; Show 7 days
	     (org-agenda-start-on-weekday nil)        ;; Start from current day
	     (org-agenda-time-grid '((daily today require-timed)
				     (800 1000 1200 1400 1600 1800 2000)
				     "......" "----------------"))  ;; Time grid config
	     (org-agenda-include-deadlines nil)       ;; No deadlines
	     (org-agenda-skip-scheduled-if-done t)     
	     (org-agenda-skip-deadline-if-done t)
	     (org-agenda-skip-scheduled-delay-if-done t)
	     (org-agenda-skip-function                ;; Skip scheduled items
	      '(org-agenda-skip-entry-if 'scheduled 'deadline))
	     (org-agenda-prefix-format '((agenda . "%?-12t ")))  ;; Only show time
	     (org-agenda-todo-keyword-format "")
	     (org-agenda-show-all-dates t)
	     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
	     (org-agenda-format-date "%A %Y-%m-%d")
	     ;; Super agenda groups
	     (org-super-agenda-groups
	      '(
		(:name "Happening today" 
		       :and(:scheduled nil :deadline nil :not(:time-grid t)))
		(:name "Today's Schedule"
		       :time-grid t)
		))))))
  ))

(setq org-agenda-time-grid-use-ampm t)
(setq org-agenda-with-times t)
(setq org-agenda-time-format "%I:%M%p")

; used with %i
;(setq org-agenda-category-icon-alist 
;      `(("bread"  ,(list (propertize "‍🥖")))
;        ("Music" ,(list (propertize "🎶" )))
;        ("Home"  ,(list (propertize"🏡" )))))

(setq org-agenda-prefix-format
'((agenda . "  - %t ")
  (tags   . "○ ")
  (todo   . "○ ")))

(use-package calfw)
(use-package google-maps)

(use-package calfw-org
  :config
  (setq cfw:org-agenda-schedule-args '(:timestamp)))

(defun my/cfw:trim-text (text)
  "Trim TEXT to fit in WIDTH, without adding ellipsis that breaks formatting."
  text)
;; Override the default truncation function
(advice-add 'cfw:trim :override #'my/cfw:trim-text)

(use-package org-modern)

;; Face customizations for org mode
(defun my/toggle-org-modern ()
  "Toggle org-modern-mode on and off."
  (interactive)
  (if (bound-and-true-p org-modern-mode)
      (progn
        (org-modern-mode -1)
        (message "org-modern-mode disabled"))
    (progn
      (org-modern-mode)
      (message "org-modern-mode enabled"))))

(global-set-key (kbd "<f2>") 'my/toggle-org-modern)

(defun my/open-inbox ()
  (interactive)
  "Opens the inbox.org file in the notes directory."
  (interactive)
  (find-file (expand-file-name "inbox.org" my-notes-directory)))
(defun my/open-inbox-mobile ()
  (interactive)
  "Opens the inbox-mobile.org file in the notes directory."
  (interactive)
  (find-file (expand-file-name "inbox-mobile.org" my-notes-directory)))
(defun my/open-emacs-config ()
  (interactive)
  "Opens the emacs.org file in the nix directory."
  (interactive)
  (find-file (expand-file-name "system/with/user/with/program/emacs.org" my-nix-systems-flake-directory)))
(defun my/open-log ()
  (interactive)
  "Opens the logs.org file in the notes directory."
  (interactive)
  (find-file (expand-file-name "logs.org" my-notes-directory)))
(defun my/open-prompts ()
  (interactive)
  "Opens the logs.org file in the notes directory."
  (interactive)
  (find-file (expand-file-name "prompts.org" my-notes-directory)))
(defun my/open-budget ()
  (interactive)
  "Opens the logs.org file in the notes directory."
  (interactive)
  (find-file (expand-file-name "budget.org" my-notes-directory)))

(defun my/get-project-choices ()
  "Return a list of available projects from Projectile."
  (if (and (fboundp 'projectile-known-projects)
           (fboundp 'projectile-project-root))
      (let ((projects (projectile-known-projects))
            (current (when (projectile-project-p)
                       (projectile-project-root))))
        ;; Put the current project at the beginning if we're in one
        (if current
            (cons current (delete current projects))
          projects))
    '("default-project")))  ;; Fallback if projectile isn't available

(defvar my/prompt-placeholder-functions
  '(("[PROJECT]" . my/get-project-choices))
  "Alist mapping placeholder strings to functions that return choices.
Each function should either return a string (for direct substitution)
or a list of strings (for completion-based selection).")

(defun my/replace-prompt-placeholders (content)
  "Replace placeholders in CONTENT string.
Placeholders are of the form [NAME], where NAME is alphanumeric or underscore.
Use functions in `my/prompt-placeholder-functions` for special placeholders.
"  (let ((regex "\[\([A-Z0-9_]+\)\]")
        (result content))
    (while (string-match regex result)
      (let* ((full (match-string 0 result))
             (name (match-string 1 result))
             (entry (assoc full my/prompt-placeholder-functions))
             (choice
              (if entry
                  (let ((res (funcall (cdr entry))))
                    (if (listp res)
                        (completing-read (format "%s: " name) res nil t)
                      res))
                (read-string (format "%s: " name))))
        ;; Replace all occurrences of this placeholder with chosen value
        (setq result (replace-regexp-in-string
                      (regexp-quote full) choice result))))
    result)))

(defun my/get-notes-files ()
  "Return a list of note files."
  (if (boundp 'my-notes-directory)
      (let* ((default-directory my-notes-directory)
             (all-files (directory-files-recursively my-notes-directory "\\.[^.]+" t)))
        ;; Return the list with relative paths
        (mapcar (lambda (file) (file-relative-name file my-notes-directory)) all-files))
    (user-error "my-notes-directory is not set")))

(setq my/prompt-placeholder-functions
  (append my/prompt-placeholder-functions
          '(("[NOTES]" . my/get-notes-files))))

(defun my/search-llm-prompts ()
  "Search for a prompt in prompts.org and insert its content at point."
  (interactive)
  (let* ((prompt-file (expand-file-name "prompts.org" my-notes-directory))
         ;; record the user’s original buffer & point
         (target-buffer (current-buffer))
         (target-marker (point-marker))
         ;; open the prompts file
         (prompt-buf (find-file-noselect prompt-file))
         candidates selected pos content)
    ;; build the candidate list from prompt-buf…
    (with-current-buffer prompt-buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ \\(.+\\)" nil t)
          (let ((heading (match-string-no-properties 1))
                (hp (point-at-bol))
                block-content)
            (save-excursion
              (when (re-search-forward "^#\\+begin_src" nil t)
                (forward-line 1)
                (let ((start (point)))
                  (when (re-search-forward "^#\\+end_src" nil t)
                    (setq block-content
                          (truncate-string-to-width
                           (replace-regexp-in-string "\n" " "
                                                     (buffer-substring-no-properties
                                                      start (line-beginning-position)))
                           200 nil nil "..."))))))



            (when block-content
              (push (cons (format "%s  [%s]" heading block-content) hp)
                    candidates))))))
    ;; pick one
    (setq selected
          (consult--read (reverse candidates)
                         :prompt "Select prompt: "
                         :category 'prompt
                         :sort nil
                         :require-match t))
    (setq pos (cdr (assoc selected candidates)))
    ;; pull out the block’s raw content
    (when pos
      (with-current-buffer prompt-buf
        (save-excursion
          (goto-char pos)
          (when (re-search-forward "^#\\+begin_src\\(?: [^[:space:]]+\\)?" nil t)
            (forward-line 1)
            (let ((start (point)))
              (when (re-search-forward "^#\\+end_src" nil t)
                (setq content
                      (buffer-substring-no-properties
                       start (line-beginning-position)))))))))
    ;; insert back in the original location
    ;; replace placeholders in the raw content
    (setq content (my/replace-prompt-placeholders content))

    (when content
      (with-current-buffer target-buffer
        (goto-char target-marker)
        (insert content)))))

(require 'ghub)

(defvar my/github-pr-file "~/notes/code-reviews.org"
  "File to store GitHub PR todos.")

(defvar my/github-pr-queries
  '(("Involved PRs" . "is:open is:pr involves:addisonbeck -author:addisonbeck")))

(defun my/pr-exists-p (url)
  "Check if PR with URL already exists in the org file."
  ;;(message "Checking for existing PR: %s" url)
  (when (file-exists-p my/github-pr-file)
    ;;(message "File exists, checking content")
    (with-temp-buffer
(insert-file-contents my/github-pr-file)
;;(message "File contents loaded")
;; Instead of using buffer positions, just check if the string exists
(string-match-p (regexp-quote url) (buffer-string)))))

(defun my/fetch-github-prs ()
  "Fetch PRs and create new org entries if they don't exist."
  (interactive)
  (message "[%s] Fetching PRs to review..." 
           (format-time-string "%H:%M:%S"))
  (let ((buf (find-file-noselect my/github-pr-file)))
    ;;(message "Buffer created: %S" buf)
    (with-current-buffer buf
;;(message "In buffer")
(org-mode)
;;(message "Org mode enabled")
(let ((max-point (point-max)))
  ;;(message "Max point: %S" max-point)
  (goto-char max-point)
  ;;(message "Moved to end of buffer")
  (dolist (query-pair my/github-pr-queries)
    (let* ((section-name (car query-pair))
	   (query (cdr query-pair)))
      ;;(message "Processing query: %s" section-name)
      (let ((response (ghub-graphql
		       "query($query: String!) {
						  search(query: $query, type: ISSUE, first: 100) {
						    nodes {
						      ... on PullRequest {
							title
							url
							repository {
							  nameWithOwner
							}
							author {
							  login
							}
							updatedAt
							state
						      }
						    }
						  }
						}"
		       `((query . ,query)))))
	;;(message "Got GraphQL response")
	(when-let ((prs (alist-get 'nodes (alist-get 'search (alist-get 'data response)))))
	  ;;(message "Found %d PRs" (length prs))
	  (dolist (pr prs)
	    ;;(message "Processing PR: %S" pr)
	    (let-alist pr
	      ;;(message "Checking if PR exists: %s" .url)
	      (let ((exists-result (my/pr-exists-p .url)))
		;;(message "PR exists check returned: %S" exists-result)
		(unless exists-result
		  ;;(message "PR doesn't exist, inserting")
		  (let ((insert-point (point)))
		    ;;(message "Current point before insert: %S" insert-point)
		    (insert (format "* TODO %s\nDEADLINE: <%s -0d>\n:PROPERTIES:\n:PR_URL: %s\n:REPO: %s\n:AUTHOR: %s\n:END:\n"
				    .title
				    (format-time-string "%Y-%m-%d")
				    .url
				    .repository.nameWithOwner
				    .author.login))
		    ;;(message "Insert completed")
        )))))))))))
    ;;(message "Saving buffer")
    (with-current-buffer buf
      (save-buffer))))
    ;;(message "PR fetch completed")))

(global-set-key (kbd "C-c g p") #'my/fetch-github-prs)

(defun remove-duplicate-org-entries ()
  (interactive)
  (let ((seen-urls (make-hash-table :test 'equal)))
    (org-map-entries
     (lambda ()
 (let ((pr-url (org-entry-get nil "PR_URL")))
   (if (and pr-url (gethash pr-url seen-urls))
       (org-cut-subtree)
     (when pr-url
       (puthash pr-url t seen-urls))))))))

(run-with-timer 0 (* 60 60) #'my/fetch-github-prs)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
	  which-key-prefix-prefix "→"
	  which-key-sort-order 'which-key-key-order-alpha
	  which-key-side-window-location 'bottom
	  which-key-side-window-max-height 0.25))

(use-package gruvbox-theme
    :ensure t
    :config
    (load-theme 'gruvbox-light-hard t))


(defvar my/current-theme 'gruvbox-light-hard
  "Current theme state, either 'gruvbox-dark-hard or 'gruvbox-light-hard.")

(defun my/toggle-theme ()
  "Toggle between gruvbox light and dark themes."
  (interactive)
  ;; First disable all custom-enabled themes to start fresh
  (mapc #'disable-theme custom-enabled-themes)
  
  ;; Toggle and load the appropriate theme
  (if (eq my/current-theme 'gruvbox-light-hard)
      (progn
        (setq my/current-theme 'gruvbox-dark-hard)
        (load-theme 'gruvbox-dark-hard t)
        (message "Switched to dark theme"))
    (progn
      (setq my/current-theme 'gruvbox-light-hard)
      (load-theme 'gruvbox-light-hard t)
      (message "Switched to light theme")))
  
  ;; Reapply all custom face settings
  (my/apply-custom-faces))

;; Make sure custom faces are applied after any theme is loaded
(advice-add 'load-theme :after
            (lambda (&rest _) (my/apply-custom-faces)))

  (custom-set-faces
   ;`(org-warning ((t (:foreground ,(if (eq 'dark (frame-parameter nil 'background-mode))
				       ;"#ffffff"  ; gruvbox-dark white
				     ;"#000000")  ; gruvbox-light black
				  ;))))
   '(org-agenda-deadline-face ((t (:inherit org-warning :foreground nil :background nil :weight bold))))
   '(org-upcoming-deadline ((t (:inherit org-warning :foreground nil :background nil :weight bold))))
   '(org-scheduled-previously ((t (:inherit org-warning :foreground nil :background nil :weight normal))))
   )

(use-package olivetti
  :config
  ;; Use both margins and fringes for fancy "page" look
  (setq olivetti-style 'fancy)
  
  ;; Define custom fringe face that works with both light/dark themes
  )

(defun my/toggle-olivetti ()
  "Toggle olivetti mode with my preferred settings."
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (olivetti-mode -1)
    (progn
      (olivetti-mode)
      (olivetti-set-width 120))))

(defun my/update-olivetti-fringe-face ()
    "Update olivetti-fringe face based on current theme."
    (let ((bg-color (if (eq my/current-theme 'gruvbox-light-hard)
                        "#f0ead8"  
                      "#161819"))) 
      (set-face-attribute 'olivetti-fringe nil :background bg-color)))

(use-package elfeed
  :ensure t
  :bind
  ("C-x w" . elfeed)
  :config
  (evil-define-key 'normal elfeed-search-mode-map
	     (kbd "r") 'elfeed-search-untag-all-unread
	     (kbd "u") 'elfeed-search-tag-all-unread
	     (kbd "RET") 'elfeed-search-show-entry
	     (kbd "q") 'quit-window
	     (kbd "g") 'elfeed-update
	     (kbd "G") 'elfeed-search-update--force)

  (evil-define-key 'normal elfeed-show-mode-map
	     (kbd "r") 'elfeed-show-untag-unread
	     (kbd "u") 'elfeed-show-tag-unread
	     (kbd "q") 'quit-window
	     (kbd "n") 'elfeed-show-next
	     (kbd "p") 'elfeed-show-prev
	     (kbd "b") 'elfeed-show-visit)

  (setq elfeed-search-filter "+unread")
  (setq elfeed-sort-order 'descending))

(use-package elfeed-protocol
  :ensure t
  :after elfeed
  :custom
  (elfeed-use-curl t)
  (elfeed-protocol-enabled-protocols '(fever))
  (setq elfeed-protocol-log-trace t)
  (elfeed-protocol-fever-update-unread-only t)
  (elfeed-protocol-fever-fetch-category-as-tag t)
  (elfeed-protocol-feeds '(("fever+https://me@homelab.rss"
		      :api-url "https://homelab.tail357e32.ts.net/rss/api/fever.php"
		      :use-authinfo t)))
  (elfeed-protocol-enabled-protocols '(fever))
  :config
  (elfeed-protocol-enable))

(defun my/elfeed-reset ()
  "Reset elfeed database and update."
  (interactive)
  (when (yes-or-no-p "Really reset elfeed database? ")
    (let ((db (expand-file-name "~/.elfeed/index"))
    (data (expand-file-name "~/.elfeed/data")))
(message "Checking paths: index=%s data=%s" db data)

;; Try to close elfeed first
(elfeed-db-unload)
(message "Database unloaded")

;; Delete files with error checking
(condition-case err
    (progn
      (when (file-exists-p db)
	(delete-file db)
	(message "Deleted index file"))
      (when (file-exists-p data)
	(delete-directory data t)
	(message "Deleted data directory")))
  (error (message "Error during deletion: %s" err)))

;; Restart elfeed
(elfeed)
(elfeed-search-update--force)
(message "Reset complete"))))

;; Set elfeed-show-entry-switch to display in a side window
(setq elfeed-show-entry-switch #'elfeed-display-buffer-right)

;; Define the display function for right split
(defun elfeed-display-buffer-right (buf)
  (let ((display-buffer-mark-dedicated t))
    (display-buffer 
     buf
     '((display-buffer-reuse-window display-buffer-in-side-window)
 (side . right)
 (window-width . 0.5)))))

;; Optional: Make elfeed respect this two-pane setup when updating
(defadvice elfeed-search-update (after configure-windows activate)
  (when (get-buffer "*elfeed-entry*")
    (elfeed-display-buffer-right (get-buffer "*elfeed-entry*"))))

;; Optional: Return focus to search buffer after showing entry
(defadvice elfeed-show-entry (after switch-to-search activate)
  (select-window (get-buffer-window "*elfeed-search*")))

;;(require 'elfeed-tube)
;;(elfeed-tube-setup)
;;(define-key elfeed-show-mode-map (kbd "F") 'elfeed-tube-fetch)
;;(define-key elfeed-show-mode-map [remap save-buffer] 'elfeed-tube-save)
;;(define-key elfeed-search-mode-map (kbd "F") 'elfeed-tube-fetch)
;;(define-key elfeed-search-mode-map [remap save-buffer] 'elfeed-tube-save)
;;(require 'elfeed-tube-mpv)
;;(define-key elfeed-show-mode-map (kbd "C-c C-f") 'elfeed-tube-mpv-follow-mode)
;;(define-key elfeed-show-mode-map (kbd "C-c C-w") 'elfeed-tube-mpv-where)
;(setq elfeed-search-title-max-width 120)
;(setq elfeed-search-title-min-width 120)
;(setq elfeed-search-date-format '("%Y/%m-%d %H:%M" :left))
;(setq elfeed-search-filter "+unread")

(use-package gptel
  :config
  ;; Enable debug logging
  ;; Token access for GitHub Copilot
  (defvar gptel-github-api-key
    (lambda ()
(when-let ((auth (car (auth-source-search
		       :host "github.copilot"
		       :require '(:secret)))))
  (let ((token (plist-get auth :secret)))
    (if (functionp token)
	(funcall token)
      token)))))

  (defun gptel-copilot--exchange-token ()
    (let* ((github-token (if (functionp gptel-github-api-key)
		       (funcall gptel-github-api-key)
		     gptel-github-api-key))
     (url-request-method "GET")
     (url-request-extra-headers
      `(("Authorization" . ,(format "Bearer %s" github-token))
	("Accept" . "application/json")))
     response-buffer token-str)
(setq response-buffer
      (url-retrieve-synchronously
       "https://api.github.com/copilot_internal/v2/token"
       t nil 30))
(when response-buffer
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (forward-char)
      (condition-case nil
	  (let ((json-response (json-read)))
	    (setq token-str (cdr (assoc 'token json-response))))
	(error nil)))
    (kill-buffer response-buffer)))
token-str))

  ;; Store the exchanged token
  (defvar gptel-copilot--exchanged-token nil)
  (setq gptel-copilot--exchanged-token (gptel-copilot--exchange-token))

  ;; Update gptel-api-key to use the exchanged token
  (setq gptel-api-key
  (lambda ()
    (or gptel-copilot--exchanged-token
	(setq gptel-copilot--exchanged-token
	      (gptel-copilot--exchange-token)))))

  ;; Advice to include full path in message
  (defun gptel--insert-at-beginning-with-path (initial-point)
    "Include full path when showing buffer contents."
    (let ((full-path (buffer-file-name)))
(goto-char initial-point)
(insert
 (format "In file %s:\n\n"
	 (if full-path
	     (expand-file-name full-path)
	   (buffer-name))))))

  (advice-add 'gptel--insert-at-beginning :override #'gptel--insert-at-beginning-with-path)

  ;; Create custom backend for GitHub Copilot
  (setq gptel-copilot-backend
  (gptel-make-openai
   "github-copilot"
   :host "api.githubcopilot.com/"
   :endpoint "chat/completions"
   :key 'gptel-api-key
   :stream t
   :models '((gpt-4o-2024-08-06 :name "gpt-4o-2024-08-06")
	     (claude-3.5-sonnet :name "claude-3.5-sonnet")
	     (claude-3.7-sonnet :name "claude-3.7-sonnet")
	     (claude-3.7-sonnet-thought :name "claude-3.7-sonnet-thought")
	     (o1-2024-12-17 :name "o1-2024-12-17")
	     (o1-mini-2024-09-12 :name "o1-mini-2024-09-12"))
   :header (lambda ()
	     `(("Authorization" . ,(format "Bearer %s" (funcall gptel-api-key)))
	       ("Content-Type" . "application/json")
	       ("Accept" . "application/json")
	       ("Copilot-Integration-Id" . "vscode-chat")
	       ("editor-version" . "vscode/1.84.2")
	       ("editor-plugin-version" . "1.138.0")
	       ("user-agent" . "GithubCopilot/1.138.0")))))

  (advice-add 'gptel--url-parse-response :around
	(lambda (orig-fun backend proc-info)
	  (let ((result (funcall orig-fun backend proc-info)))
	    (when (and (stringp (cadr result))
		       (string-match-p "HTTP/2 401" (cadr result)))
	      (message "Token expired, refreshing and retrying...")
	      (setq gptel-copilot--exchanged-token nil)
	      ;; Get new token
	      (funcall gptel-api-key)
	      ;; Retry the request
	      (let ((request-data (plist-get proc-info :request-data)))
		(when request-data
		  (gptel-request request-data))))
	    result)))

  (defun test-gptel-token-refresh ()
    "Test gptel token refresh logic."
    (interactive)
    (message "=== Starting Token Test ===")
    (message "Current token (first 50 chars): %s..."
       (substring gptel-copilot--exchanged-token 0 50))
    ;; Force token refresh by setting to nil
    (setq gptel-copilot--exchanged-token nil)
    ;;(message "Cleared token, making request...")
    ;; Make request that should trigger token refresh
    ;;(gptel-request
    ;;"Test message"
    ;;:callback (lambda (response info)
    ;;(message "=== Request completed ===")
    ;;(message "New token (first 50 chars): %s..."
    ;;(substring gptel-copilot--exchanged-token 0 50))
    ;;(message "Response status: %s" (plist-get info :status))
    ;;(message "Got response: %s" response)))
    )

  (defun get-anthropic-api-key ()
    (when-let ((auth (car (auth-source-search
		     :host "api.anthropic.com"
		     :require '(:secret)))))
(let ((token (plist-get auth :secret)))
  (if (functionp token)
      (funcall token)
    token))))

  (defun get-openai-api-key ()
    (when-let ((auth (car (auth-source-search
		     :host "api.openai.com"
		     :require '(:secret)))))
(let ((token (plist-get auth :secret)))
  (if (functionp token)
      (funcall token)
    token))))

  (defun get-gemini-api-key ()
    (when-let ((auth (car (auth-source-search
		     :host "api.gemini.com"
		     :require '(:secret)))))
(let ((token (plist-get auth :secret)))
  (if (functionp token)
      (funcall token)
    token))))

  (gptel-make-anthropic "Claude"          
		  :stream t
		  :key #'get-anthropic-api-key)

  (gptel-make-gemini "Gemini"          
	       :key #'get-gemini-api-key)

  (gptel-make-openai "ChatGPT"          
	       :key #'get-openai-api-key
         :stream t
         :models gptel--openai-models)

  ;;(gptel-make-anthropic "claude" 
  ;;:key #'get-anthropic-api-key
  ;;:stream t
  ;;:models '(claude-3-7-sonnet-20250219)
  ;;:header (lambda () (when-let* ((key (gptel--get-api-key)))
  ;;`(("x-api-key" . ,key)
  ;;("anthropic-version" . "2023-06-01")
  ;;("anthropic-beta" . "pdfs-2024-09-25")
  ;;("anthropic-beta" . "output-128k-2025-02-19")
  ;;("anthropic-beta" . "prompt-caching-2024-07-31"))))
  ;;:request-params '(:max_tokens 4096))

  ;;(gptel-make-anthropic "claude-thinking" 
  ;;:key #'get-anthropic-api-key
  ;;:stream t
  ;;:models '(claude-3-7-sonnet-20250219)
  ;;:header (lambda () (when-let* ((key (gptel--get-api-key)))
  ;;`(("x-api-key" . ,key)
  ;;("anthropic-version" . "2023-06-01")
  ;;("anthropic-beta" . "pdfs-2024-09-25")
  ;;("anthropic-beta" . "output-128k-2025-02-19")
  ;;("anthropic-beta" . "prompt-caching-2024-07-31"))))
  ;;:request-params '(:thinking (:type "enabled" :budget_tokens 2048)
  ;;:max_tokens 4096))

  (setq gptel-log-level 'debug)
  ;; Use org-mode for gptel buffers
  (setq gptel-default-mode 'org-mode)
  ;; Enable branching conversations in org-mode
  (setq gptel-org-branching-context t)
  (setq gptel-confirm-tool-calls t)
  (setq gptel-include-tool-results t)

  ;; Update prompt/response prefixes for org-mode to be compatible with branching conversations
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  (setq gptel-backend gptel-copilot-backend)
  (setq gptel-model 'claude-3.7-sonnet)
  (setq gptel-default-mode 'org-mode)
(gptel-make-preset 'recipe-assistant
  :description "Recipe assistant"
  :system "You are a Recipe Assistant for a competent home cook with a family of 6 (including 4 young children). Your goal is to provide flexible cooking frameworks rather than rigid recipes, offering ingredient substitutions and adaptations while keeping meals budget-friendly and reasonably healthy.

When responding:
- Speak as if you are simply outputting text that can be used as a tutorial for a framework for the dish being discussed. Do not address the user. Your output should be the perfect guide based on the dish being discussed.
- If asked to make edits only output the modified sections of the guide
- Focus on adaptable cooking methods that allow for variations based on available ingredients
- Suggest ways to prep components ahead of time for efficient weeknight cooking
- Provide substitution options for ingredients, especially expensive ones
- Consider the user's preferred cooking equipment: cast iron skillet, carbon steel wok, two-burner griddle, dutch ovens, Vitamix, stand mixer, air fryer, Instant Pot, waffle maker, mandolin, pizza steel.
- Include tips for batch preparation when appropriate
- Balance kid-friendly flavors with nutritional value
- Provide guidance for meal planning when requested, including grocery lists
- Highlight techniques that offer the best return on effort (making things from scratch only when it significantly elevates the dish)
- Use metric weights"
  :tools '("mcp-brave-search" "mcp-fetch" "mcp-filesystem" "mcp-server-text-editor" "shell_command" "system")) 
(gptel-make-preset 'default
  :description "Default"
  :system "You are a large language model living in emacs and a helpful assistant. Responsd concisely."
  :tools '("mcp-fetch github-mcp")) 
;; Notes from use
;; This worked well, but the file reading tools are flakey. search_files
;; seems to only work one directory deep, but the LLM kept expecting it to be
;; recursive. Urging it to use rg via search_command helped.
;; The main thing I learned form this is to cold abort the LLM if it's off
;; track in its reasoning/tool use and starts wasting time.
(gptel-make-preset 'zoom-bug-error
  :description "Bitwarden Desktop App Bug Fixer"
  :backend "ChatGPT"
  :model 'o4-mini
  :system "You are a large language model integrated into emacs. We are using org syntax to chat. You are tasked with triaging and patching a bug in the Bitwarden password manager's desktop application. Please use the availible tools and key files to assert the cause of the bug. When you have an idea what the root cause is please propose a fix. Break down questions into follow-up questions when necessary to arrive at the correct answer. Show the steps you followed in reaching the answer.

*Key File Locations:*

- =/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/zoom-settings-bug.org=: this is our memory file about the task at hand. It contains all the known information about the bug, and you should make frequent edits to it to include notes, context, or anything that would be helpful for a human or LLM to remember.
- =/Users/me/binwarden/bitwarden-clients/zoom-settings-bug/=: This is a directory with a cloned and up to date copy of the Bitwarden =clients= monorepo source code. It contains the desktop apps source code as well as other Bitwarden clients and shared libraries between them. It is very large and easy to get lost in. Please record key directory structures and file paths to our memory file.

After receiving tool results, carefully reflect on their quality and determine optimal next steps before proceeding.

Use your thinking to plan and iterate based on this new information, and then take the best next action.

Whenever you need data:
1. PLAN
  - Restate your goal.
  - Choose the single best tool for that goal, citing capabilities.
  - Write down the exact arguments you’ll pass.
2. EXECUTE
  - Call the tool with precisely those arguments.
3. REFLECT
  - Check raw output for success: Is it empty?  Did the path exist?  Did I get what I expected?
  - If OK, parse and continue.  If not, pick a fallback tool or refine arguments.
  - Record what you tried, what worked or failed, then decide next step.

Example:
“Goal: find the newest file in ~/Downloads by modified date.
    PLAN:
    - I need a reverse-time sort. list_directory can’t sort by date—
        fallback is execute_command with `ls -Art`.
    - Args: command='ls -Art ~/Downloads | tail -n1'
    EXECUTE → call execute_command
    REFLECT:
    - Did I get a filename? If yes, capture it. If no, check path or switch to `find ... -printf '%T@ %p\n'`.

Before you start, please ask me any questions you have about this so I can give you more context.

Be extremely comprehensive
"
  :tools '("mcp-fetch" "mcp-github-mcp" "mcp-mcp-server-text-editor" "mcp-brave-search" "system" "mcp-filesystem")) 
  )



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
(define-key gptel-mode-map (kbd "C-c C-m") #'my/gptel-minify-context)

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

;; These can be really flakey. If one goes down, they all go down, and finding which one really broke can be hard.
(setq mcp-hub-servers
      `(
       ("filesystem" . (:command "npx"
                                       :args
; Icloud
                                       ("-y" "@modelcontextprotocol/server-filesystem" "/Users/me/nix" "/Users/me/binwarden" "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes")))
        ;; https://github.com/bhouston/mcp-server-text-editor - same functionality as Claude's text editor
        ;; PSA: this service requires absolute paths to be used by the LLM

        ("brave-search" . (:command "npx"
                                         :args ("-y" "@modelcontextprotocol/server-brave-search")
                                         :env (:BRAVE_API_KEY ,(my/get-brave-token))))
        ("mcp-server-text-editor" . (:command "npx"
                                            :args ("-y" "mcp-server-text-editor")))
        ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
        ("memory" . (:command "npx"
          :args ("-y" "@modelcontextprotocol/server-memory")))
            ;("desktop-commander" . (:command "npx"
                                    ;:args ("-y" "@wonderwhy-er/desktop-commander")))
        ("github-mcp" . (:command "docker"
                        :args ("run" "-i" "--rm"
                              "-e" "GITHUB_PERSONAL_ACCESS_TOKEN"
                              "ghcr.io/github/github-mcp-server")
                       :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(my/get-github-mcp-token))))
        ))

;; Start MCP servers after Emacs initializes
(add-hook 'after-init-hook #'mcp-hub-start-all-server)

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" (get-anthropic-api-key))
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))

(defun copy-file-path ()
  "Copy the current buffer file path to the kill ring."
  (interactive)
  (let ((filepath (buffer-file-name)))
    (when filepath
	(kill-new filepath)
	(message "Copied: %s" filepath))))

(defun copy-file-name ()
  "Copy the current buffer file name to the kill ring."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (when filename
	(kill-new filename)
	(message "Copied: %s" filename))))

(defun copy-directory-path ()
  "Copy the current buffer directory path to the kill ring."
  (interactive)
  (let ((dirpath (file-name-directory (buffer-file-name))))
    (when dirpath
	(kill-new dirpath)
	(message "Copied: %s" dirpath))))

(require 'avy)
(define-key evil-normal-state-map (kbd "s") 'avy-goto-char-timer)

(require 'rg)

(use-package ansi-color
  :config
  (defun my/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
	(ansi-color-apply-on-region
	 compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'my/colorize-compilation)

  (setq ansi-color-for-comint-mode t)
  (setq comint-terminfo-terminal "xterm-256color"))

(defun bitwarden/nx-poc-npm-i ()
  "Run npm ci in the nx PoC"
  (interactive)
  (let* ((default-directory (my/get-project-path "nx-poc"))
	   (compilation-buffer-name-functionl 
	    (lambda (_mode) (format "*nx-poc-npm-i*"))))
    (compile "npm i" t)))

(defun bitwarden/run-nx-poc-web ()
  "Build the web vault of the nx poc project with a uniquely named buffer."
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "nx-poc") "/apps/web"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-poc-web-build*"))))
    (compile "npm run build:watch" t)))

(defun bitwarden/run-nx-poc-browser-chrome ()
  "Build the chrome extension of the nx poc project with a uniquely named buffer."
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "nx-poc") "/apps/browser"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-poc-chrome-build*"))))
    (compile "npm run build:watch:chrome" t)))

(defun bitwarden/run-nx-poc-browser-firefox ()
  "Build the chrome extension of the nx poc project with a uniquely named buffer."
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "nx-poc") "/apps/browser"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-poc-firefox-build*"))))
    (compile "npm run build:watch:firefox" t)))

(defun bitwarden/run-nx-poc-desktop ()
  "Build the desktop applicaton in the nx poc project with a uniquely named buffer."
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "nx-poc") "/apps/desktop"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-poc-desktop-build*"))))
    (compile "npm run build:watch" t)))

(defun bitwarden/build-nx-poc-cli ()
  "Build the cli in the nx poc project with a uniquely named buffer."
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "nx-poc") "/apps/cli"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-poc-cli-build*"))))
    (compile "npm run build" t)))

(defun bitwarden/nx-poc-nx-report ()
  "Runs nx report in the poc project"
  (interactive)
  (let* ((default-directory (my/get-project-path "nx-poc"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-poc-nx-report*"))))
    (compile "npx nx report" t)))

(defun bitwarden/nx/build (target)
  "Build the NX poc using NX for TARGET"
  (interactive
   (list (completing-read "Target to build: " 
			    '("common" "angular" "web" "cli" "desktop" "browser")
			    nil nil nil nil "common")))
  (let* ((default-directory (my/get-project-path "nx-poc"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-pox-%s-build*" target))))
    (compile (format "npx nx build %s" target) t)))

(defun bitwarden/nx/serve (target)
  "Serve the NX poc using NX for TARGET"
  (interactive
   (list (completing-read "Target to build: " 
			    '("web" "desktop" "browser")
			    nil nil nil nil "web")))
  (let* ((default-directory (my/get-project-path "nx-poc"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-pox-%s-serve*" target))))
    (compile (format "npx nx serve %s" target) t)))

(defun bitwarden/nx/start (target)
  "Start the NX poc using NX for TARGET"
  (interactive
   (list (completing-read "Target to build: " 
			    '("cli")
			    nil nil nil nil "cli")))
  (let* ((default-directory (my/get-project-path "nx-poc"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-pox-%s-start*" target))))
    (compile (format "npx nx start %s --verbose" target) t)))

(defun bitwarden/nx/cleanup ()
  "Clean up the nx poc project"
  (interactive)
  (let* ((default-directory (my/get-project-path "nx-poc"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nx-poc-cleanup*"))))
    (compile "rm -rf node_modules ; rm -rf .nx" t)))

(defun my/nix/rebuild (system)
  "Rebuild my nix config for the specified SYSTEM."
  (interactive
   (list (completing-read "System to rebuild: " 
			    '("air" "bw")
			    nil nil nil nil "air")))
  (let* ((default-directory (my/get-project-path "nix"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nix-%s-rebuild*" system))))
    (compile (format "nix develop --command rebuild %s" system) t)))

(defun my/nix/format ()
  "Run the formatters in my nix systems configuration"
  (interactive)
  (let* ((default-directory (my/get-project-path "nix"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nix-systems-format*"))))
    (compile "nix develop --command apply formatting" t)))

(defun my/nix/commit (message)
  "Commit all files in my nix config with MESSAGE"
  (interactive
   (list (read-string "Commit message: " nil nil nil)))
  (let* ((default-directory (my/get-project-path "nix"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nix-systems-commit*"))))
    (compile (format "git add . ; git commit -m %s ; git pull ; git push" message) t)))

(defun my/nix/update-flake-lock ()
  "Update flake lock in my nix systems config"
  (interactive)
  (let* ((default-directory (my/get-project-path "nix"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nix-systems-flake-lock-update*"))))
    (compile "nix flake update" t)))

(defun my/nix/check-status ()
  "Check the git status of my nix systems config"
  (interactive)
  (let* ((default-directory (my/get-project-path "nix"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nix-systems-git-status*"))))
    (compile "git status" t)))

(defun my/nix/update-minecraft-packwize ()
  "Update the pacckages for the packwiz server for my kids"
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "nix") "/packwiz/bonesfamily"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*nix-systems-packwiz-packages*"))))
    (compile "packwiz " t)))

(defun my/quick-commit (message)
  "Commit all files in my notes with MESSAGE"
  (interactive
   (list (read-string "Commit message: " nil nil nil)))
  (let* ((default-directory (my/get-project-path "notes"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*notes-commit*"))))
    (compile (format "git add . ; git commit -m %s ; git pull ; git push" message) t)))

(defun bitwarden/clients/npm/ci-run ()
  "Run the typeschecker for the clients monorepo"
  (interactive)
  (let* ((default-directory (my/get-project-path "clients"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*clients-typechecker*"))))
    (compile "npm ci" t)))

(defun bitwarden/clients/typechecker/run ()
  "Run the typeschecker for the clients monorepo"
  (interactive)
  (let* ((default-directory (my/get-project-path "clients"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*clients-typechecker*"))))
    (compile "npm run test:types" t)))

(defun bitwarden/clients/browser/chrome/run ()
  "Watch a chrome dev build of the extension"
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "clients") "/apps/browser"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*clients-browser-chrome-run*"))))
    (compile "npm run build:watch:chrome" t)))

(defun bitwarden/clients/web/run ()
  "Watch a build of the web vault"
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "clients") "/apps/web"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*clients-web-run*"))))
    (compile "npm run build:watch" t)))

(defun bitwarden/server/api/run ()
  "Watch a build of the bitwarden server api"
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "server") "/src/Api"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*bitwarden-server-api-run*"))))
    (compile "dotnet run" t)))

(defun bitwarden/server/identity/run ()
  "Watch a build of the bitwarden server identity"
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "server") "/src/Identity"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*bitwarden-server-identity-run*"))))
    (compile "dotnet run" t)))

(defun bitwarden/server/identity/run ()
  "Watch a build of the bitwarden server identity"
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "server") "/src/Identity"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*bitwarden-server-identity-run*"))))
    (compile "dotnet run" t)))

(defun bitwarden/server/run-sql ()
  "Watch a build of the bitwarden server identity"
  (interactive)
  (let* ((default-directory (concat (my/get-project-path "server") "/src/dev"))
	   (compilation-buffer-name-function 
	    (lambda (_mode) (format "*bitwarden-server-identity-run*"))))
    (compile "dotnet run" t)))

(defun bitwarden/clients/run-tests (&optional test-pattern)
  "Run Jest tests for the clients monorepo.
    If TEST-PATTERN is provided, filter tests using the -t option."
  (interactive "sTest pattern (optional): ")
  (let* ((default-directory (my/get-project-path "clients"))
   (command (if (and test-pattern (not (string-empty-p test-pattern)))
		(format "npm run test -- -t '%s'" test-pattern)
	      "npm run test"))
   (buffer-name (if (and test-pattern (not (string-empty-p test-pattern)))
		    (format "*clients-jest-%s*" test-pattern)
		  "*clients-jest*"))
   (compilation-buffer-name-function 
    (lambda (_mode) buffer-name)))
    (compile command t)))

(setq ring-bell-function 'ignore)
(defun bitwarden/clients/run-tests-interactive (&optional test-pattern)
  "Run Jest tests for the clients monorepo in interactive mode.
If TEST-PATTERN is provided, filter tests using the -t option."
  (interactive "sTest pattern (optional): ")
  (let* ((default-directory (my/get-project-path "clients"))
         (buffer-name (if (and test-pattern (not (string-empty-p test-pattern)))
                       (format "*clients-jest-watch-%s*" test-pattern)
                     "*clients-jest-watch*"))
         (command (if (and test-pattern (not (string-empty-p test-pattern)))
                    (format "npm run test:watch -- -t '%s'" test-pattern)
                  "npm run test:watch")))
    
    ;; Create or switch to the terminal buffer
    (let ((buf (get-buffer buffer-name)))
      (if buf
          (switch-to-buffer buf)
        (ansi-term (getenv "SHELL") buffer-name)))
    
    ;; Change to project directory and run command
    (term-send-string 
     (get-buffer-process buffer-name)
     (format "cd %s && %s\n" default-directory command))))

(defun bitwarden/clients/lint/run ()
  "Run linter for the clients monorepo"
  (interactive)
  (let* ((default-directory (my/get-project-path "clients"))
         (compilation-buffer-name-function 
          (lambda (_mode) (format "*clients-lint*"))))
    (compile "npm run lint" t)))

(defun scan-worktree-projects ()
  "Scan for all worktree directories and add them to Projectile."
  (interactive)
  (let ((base-dir (expand-file-name "~/binwarden/")))
    (dolist (owner-repo (directory-files base-dir t "^[^.]"))
      (when (file-directory-p owner-repo)
        (dolist (branch-dir (directory-files owner-repo t "^[^.]"))
          (when (and (file-directory-p branch-dir)
                     (file-exists-p (expand-file-name ".git" branch-dir)))
            (projectile-add-known-project branch-dir)))))))

(with-eval-after-load 'projectile
  (scan-worktree-projects))

(defun bitwarden/scan-worktrees (project-type)
  "Scan for worktrees of PROJECT-TYPE (e.g., 'clients' or 'server')."
  (let ((worktrees '())
        (base-dir (expand-file-name "~/binwarden/")))
    (dolist (dir (directory-files base-dir t "^[^.]"))
      (when (and (file-directory-p dir)
                 (string-match-p project-type (file-name-nondirectory dir)))
        (dolist (branch-dir (directory-files dir t "^[^.]"))
          (when (and (file-directory-p branch-dir)
                     (file-exists-p (expand-file-name ".git" branch-dir)))
            (push (cons (format "%s:%s" 
                                (file-name-nondirectory dir) 
                                (file-name-nondirectory branch-dir))
                        branch-dir)
                  worktrees)))))
    worktrees))

(defun bitwarden/select-worktree (project-type)
  "Select a worktree of PROJECT-TYPE with completion."
  (let* ((worktrees (bitwarden/scan-worktrees project-type))
         (selection (completing-read 
                     (format "Select %s worktree: " project-type)
                     (mapcar #'car worktrees))))
    (cdr (assoc selection worktrees))))

(defun bitwarden/clients/browser/chrome/run-in-worktree ()
  "Run Chrome extension build in a selected clients worktree."
  (interactive)
  (let* ((worktree-dir (bitwarden/select-worktree "bitwarden-clients"))
         (default-directory (concat worktree-dir "/apps/browser"))
         (compilation-buffer-name-function 
          (lambda (_mode) (format "*clients-browser-chrome-run*"))))
    (compile "pnpm run build:watch:chrome" t)))

(defun bitwarden/clients/typechecker/run-in-worktree ()
  "Run typechecker in a selected clients worktree."
  (interactive)
  (let* ((worktree-dir (bitwarden/select-worktree "bitwarden-clients"))
         (default-directory worktree-dir)
         (compilation-buffer-name-function 
          (lambda (_mode) (format "*clients-typechecker*"))))
    (compile "pnpm run test:types" t)))

;; Template for other commands
(defun bitwarden/clients/run-tests-in-worktree (&optional test-pattern)
  "Run Jest tests in a selected clients worktree.
If TEST-PATTERN is provided, filter tests using the -t option."
  (interactive "sTest pattern (optional): ")
  (let* ((worktree-dir (bitwarden/select-worktree "bitwarden-clients"))
         (default-directory worktree-dir)
         (command (if (and test-pattern (not (string-empty-p test-pattern)))
                    (format "npm run test -- -t '%s'" test-pattern)
                  "npm run test"))
         (buffer-name (if (and test-pattern (not (string-empty-p test-pattern)))
                         (format "*clients-jest-%s*" test-pattern)
                       "*clients-jest*"))
         (compilation-buffer-name-function 
          (lambda (_mode) buffer-name)))
    (compile command t)))

(defun bitwarden/clients/npm/ci-run-in-worktree ()
  "Run pnpm ci in a selected clients worktree."
  (interactive)
  (let* ((worktree-dir (bitwarden/select-worktree "bitwarden-clients"))
         (default-directory worktree-dir)
         (compilation-buffer-name-function 
          (lambda (_mode) (format "*clients-npm-ci*"))))
    (compile "npm ci" t)))

(defun bitwarden/clients/lint/worktree ()
  "Run linting in a specific clients worktree"
  (interactive)
  (let* ((worktree-dir (bitwarden/select-worktree "bitwarden-clients"))
         (default-directory worktree-dir)
         (compilation-buffer-name-function 
          (lambda (_mode) (format "*clients-npm-lint*"))))
    (compile "npm run lint" t)))

(direnv-mode)

(defun my/worktree-build ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile (or (getenv "BUILD_CMD") "make"))))

(defun my/worktree-test ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile (or (getenv "TEST_CMD") "make test"))))

(defun binwarden/create-worktree ()
  "Create a new worktree in the binwarden directory.
  1. Select a repository from binwarden directory
  2. Enter a name for the new worktree
  3. Optionally select a source branch (sorted by recent commit date)
  4. Run \'just create-worktree\' with those parameters"
  (interactive)
  (let* ((binwarden-dir (expand-file-name "~/binwarden/"))
         (repos (directory-files binwarden-dir nil "^[^.]"))
         (selected-repo (completing-read "Select repository: " repos))
         (worktree-name (read-string "Worktree name: "))
         ;; Parse the repo name to find the primary branch from .env file
         (owner (car (split-string selected-repo "-")))
         (repo (cadr (split-string selected-repo "-")))
         (env-var-name (format "PRIMARY_%s_%s_WORKTREE" 
                               (upcase owner) (upcase repo)))
         (env-file (expand-file-name ".env" binwarden-dir))
         (primary-branch 
          (when (file-exists-p env-file)
            (with-temp-buffer
              (insert-file-contents env-file)
              (when (re-search-forward (concat "^" env-var-name "=\\(.*\\)$") nil t)
                (match-string 1)))))
         ;; Set directory to the primary branch worktree for getting branch list
         (primary-worktree-dir 
          (expand-file-name (concat selected-repo "/" (or primary-branch "main")) 
                           binwarden-dir))
         ;; Get branches sorted by commit date from the primary worktree
         (default-directory primary-worktree-dir)
         (branches-raw (shell-command-to-string "git branch --sort=-committerdate"))
         ;; Clean up branch names - remove any leading symbols (*, +, etc) and whitespace
         (branches (mapcar (lambda (branch) 
                            (string-trim (replace-regexp-in-string "^[\\*\\+ ]+" "" branch)))
                          (split-string branches-raw "\n" t)))
         ;; Prompt for optional source branch
         (from-branch (completing-read "Source branch (optional, empty for default): " 
                                      branches nil nil nil nil ""))
         (from-branch-arg (if (string-empty-p from-branch)
                             ""
                           (format " %s" from-branch)))
         (compilation-buffer-name-function 
          (lambda (_mode) (format "*create-worktree-%s-%s*" selected-repo worktree-name))))
    (compile (format "cd %s && just create-worktree %s %s%s" 
                    (shell-quote-argument binwarden-dir)
                    selected-repo worktree-name from-branch-arg) t)))

(defun reset-file-to-revision ()
  "Reset the current buffer's file to a specified revision using Magit."
  (interactive)
  (require 'magit)
  (let* ((file-path (buffer-file-name))
	   (default-directory (magit-toplevel))
	   (revision (magit-read-branch-or-commit "Reset file to revision")))
    (when (and file-path revision)
	(let ((relative-file-path (file-relative-name file-path default-directory)))
	  (magit-run-git "checkout" revision "--" relative-file-path)
	  (revert-buffer t t t)
	  (message "File reset to %s" revision)))))

(defun my/run-gh-pr-checks ()
  "Run 'gh pr checks' for the current PR with better formatting."
  (interactive)
  (when (eq major-mode 'forge-pullreq-mode)
    (let* ((pr (forge-current-topic))
           (pr-number (oref pr number))
           (buffer-name (format "*gh-pr-checks:#%s*" pr-number))
           (cmd (format "gh pr checks %s --json name,state,link" pr-number)))
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (shell-command cmd (current-buffer))
          (goto-char (point-min))
          (let* ((json-data (json-read))
                 (checks (append json-data nil))
                 (passed 0)
                 (failed 0)
                 (pending 0)
                 (failed-jobs '()))
            
            ;; Count statuses and collect failed jobs
            (dolist (check checks)
              (let ((state (cdr (assoc 'state check))))
                (cond
                 ((string= state "SUCCESS") (cl-incf passed))
                 ((string= state "FAILURE") 
                  (cl-incf failed)
                  (push check failed-jobs))
                 (t (cl-incf pending)))))
            
            ;; Clear and format buffer
            (erase-buffer)
            (insert (propertize (format "PR #%s Checks Summary\n\n" pr-number)
                               'face '(:weight bold :height 1.2)))
            (insert (format "Total: %d | " (length checks)))
            (insert (propertize (format "Passed: %d | " passed)
                               'face '(:foreground "green")))
            (insert (propertize (format "Failed: %d | " failed)
                               'face '(:foreground "red" :weight bold)))
            (insert (propertize (format "Pending: %d\n\n" pending)
                               'face '(:foreground "orange")))
            
            ;; Add detailed listing
            (insert (propertize "All Checks:\n" 'face '(:weight bold)))
            (dolist (check checks)
              (let* ((name (cdr (assoc 'name check)))
                     (state (cdr (assoc 'state check)))
                     (link (cdr (assoc 'link check)))
                     (state-face (cond
                                 ((string= state "SUCCESS") '(:foreground "green"))
                                 ((string= state "FAILURE") '(:foreground "red"))
                                 (t '(:foreground "orange")))))
                (insert "• ")
                (insert (propertize (format "%-50s" (truncate-string-to-width name 50))
                                   'face '(:weight bold)))
                (insert " - ")
                (insert (propertize state 'face state-face))
                (when link
                  (insert " [")
                  (insert-text-button "Link"
                                     'action (lambda (_) (browse-url link))
                                     'follow-link t)
                  (insert "]"))
                (insert "\n")))
            
            ;; Add failed jobs section
            (when failed-jobs
              (insert "\n")
              (insert (propertize "Failed Jobs:\n" 
                                 'face '(:foreground "red" :weight bold)))
              (dolist (job failed-jobs)
                (let ((name (cdr (assoc 'name job)))
                      (link (cdr (assoc 'link job))))
                  (insert "• ")
                  (insert (propertize name 'face '(:foreground "red")))
                  (when link
                    (insert " → ")
                    (insert-text-button "Open in Browser"
                                      'action (lambda (_) (browse-url link))
                                      'follow-link t))
                  (insert "\n")))))
          
          (special-mode)
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun my/rerun-failed-gh-pr-checks ()
  "Rerun failed checks for the current PR using GitHub CLI."
  (interactive)
  (when (eq major-mode 'forge-pullreq-mode)
    (let* ((pr (forge-current-topic))
           (pr-number (oref pr number))
           ;; Use the JSON format that matches what gh pr checks outputs
           (cmd (format "gh pr checks %s --json name,databaseId,status,conclusion" pr-number))
           failed-jobs)
      
      ;; Get failed jobs
      (message "Fetching checks for PR #%s..." pr-number)
      (let ((json-output (shell-command-to-string cmd)))
        (condition-case err
            (let ((json-object (json-read-from-string json-output)))
              (setq failed-jobs
                    (seq-filter (lambda (job)
                                  (and (alist-get 'conclusion job nil nil #'equal)
                                       (string= (alist-get 'conclusion job) "failure")))
                                json-object)))
          (error
           (message "Error parsing JSON: %S\nOutput was: %s" err (substring json-output 0 100))
           (setq failed-jobs nil))))
      
      (if (null failed-jobs)
          (message "No failed jobs to rerun!")
        (when (yes-or-no-p (format "Rerun %d failed check(s)? " (length failed-jobs)))
          (let ((rerun-buffer (get-buffer-create "*gh-rerun-checks*"))
                (counter 0))
            (with-current-buffer rerun-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (propertize "Rerunning failed checks...\n\n" 'face '(:weight bold)))
                
                (dolist (job failed-jobs)
                  (let* ((name (alist-get 'name job))
                         (id (alist-get 'databaseId job))
                         (rerun-cmd (format "gh run rerun %s" id)))
                    (insert (format "• Rerunning: %s (ID: %s)..." name id))
                    (let ((result (shell-command-to-string rerun-cmd)))
                      (if (string-match-p "Failed\\|Error" result)
                          (insert (propertize " Failed\n" 'face '(:foreground "red")))
                        (progn
                          (cl-incf counter)
                          (insert (propertize " Triggered\n" 'face '(:foreground "green")))))))
                
                (insert (propertize (format "\nSuccessfully triggered %d/%d job reruns." 
                                           counter (length failed-jobs))
                                   'face '(:weight bold)))
                (special-mode)
                (goto-char (point-min))
                (display-buffer (current-buffer)))))))))))

(require 'gnus)
(require 'smtpmail)
(require 'message)
(require 'oauth2) ;; For OAuth2 support with Gmail

(setq user-full-name "Addison Beck")
(setq user-mail-address "me@addisonbeck.com")

;; Show all messages in all groups
(setq gnus-parameters
      '((".*" (display . all))))  

;; Show all groups, including empty ones
(setq gnus-permanently-visible-groups ".*")

;; This shows all kinds of neat but verbose and annoying header information
;;(setq gnus-show-all-headers t)

;; Make 'A r' (gnus-summary-refer-article) the default listing function
(setq gnus-summary-goto-unread nil)

;; Always start with 'A A' behavior (show all articles)
(add-hook 'gnus-select-group-hook 'gnus-group-list-all-groups)

  ;;; Main Select Method (Primary Account)
(setq gnus-select-method
      '(nnimap "primary-account"
        (nnimap-address "box.addisonbeck.com")
        (nnimap-server-port 993)
        (nnimap-stream ssl)
        (nnmail-expiry-wait immediate)))

;;; OAuth2 setup for Gmail
(defun get-gmail-oauth2-token ()
  "Get OAuth2 access token for Gmail."
  (let* ((auth-info (nth 0 (auth-source-search :host "oauth2.googleapis.com" 
                                              :user "935901585839-b1c4q3mmjb4tuutgpd3aratopq7tf85k.apps.googleusercontent.com" 
                                              :service "oauth2")))
         (client-id (plist-get auth-info :user))
         (client-secret (let ((secret (plist-get auth-info :secret)))
                         (if (functionp secret)
                             (funcall secret)
                           secret)))
         (token (oauth2-token-access-token
                (oauth2-refresh-access
                 (oauth2-auth-and-store
                  "https://accounts.google.com/o/oauth2/auth"
                  "https://oauth2.googleapis.com/token"
                  client-id
                  client-secret
                  "https://mail.google.com/" nil)))))
    token))

;; Override auth function for Gmail accounts
(defun gmail-oauth2-auth (server)
  "Return the OAuth2 string for SERVER."
  (when (string-match "imap.gmail.com" server)
    (let ((token (get-gmail-oauth2-token)))
      (when token
        (concat "user=addison@bitwarden.com\001auth=Bearer " 
                token "\001\001")))))

;; Register auth function
;;(add-to-list 'nnimap-authenticator-alist
             ;;'(gmail-oauth2 gmail-oauth2-auth))

;; Use the OAuth2 authenticator with Gmail
(setq nnimap-authinfo-file "~/.authinfo")
;(setq nnimap-authenticator 'gmail-oauth2)

(defun nnimap-xoauth2-oauth2-request (user server)
  "Return the OAuth2 string for USER on SERVER."
  (when (string-match "imap.gmail.com" server)
    (let ((token (get-gmail-oauth2-token)))
      (when token
        (concat "user=" user "\001auth=Bearer " token "\001\001")))))

;;; Secondary Accounts
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "work-gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-authenticator xoauth2)  
               (nnimap-stream ssl)
               (nnmail-expiry-target "nnimap+work-gmail:[Gmail]/Trash")
               (nnmail-expiry-wait immediate)))

;;; Add more accounts as needed
;;(add-to-list 'gnus-secondary-select-methods
             ;;'(nnimap "personal"
               ;;(nnimap-address "imap.personal.com")
               ;;(nnimap-server-port 993)
               ;;(nnimap-stream ssl)))

;;; SMTP configuration with account selection
(require 'smtpmail-multi)

;; Define email accounts
(setq smtpmail-multi-accounts
      '((personal . ("me@addisonbeck.com"
                    "box.addisonbeck.com"
                    465
                    "me@addisonbeck.com"
                    nil
                    starttls))
        (work-gmail . ("addison@bitwarden.com"
                      "smtp.gmail.com" 
                      587
                      "addison@bitwarden.com"
                      nil
                      starttls))))

;; Set default account
(setq smtpmail-multi-default-account 'personal)

;; Use smtpmail-multi as the send function
(setq send-mail-function 'smtpmail-multi-send-it
      message-send-mail-function 'smtpmail-multi-send-it)

;;; Posting Styles - automatically set From, signature, etc. based on context
(setq gnus-posting-styles
      '((".*" ;; Default style
         (name "Addison Beck")
         (address "me@addisonbeck.com")
         (signature "Thanks,\nAddison"))
        ("work-gmail"
         (name "Addison Beck") 
         (address "addison@bitwarden.com")
         (organization "Bitwarden"))
        ;; Match based on recipient address
        ((header "to" "client@example\\.com")
         (address "work@gmail.com")
         (signature "Professional signature for clients"))
        ;; Add more context-specific styles as needed
        ))

;;; Gmail-specific settings
(setq gnus-parameters
      '(("work-gmail"
         (display . all)
         (posting-style
          (name "Addison Beck")
          (address "addison@bitwarden.com")
          (signature "Thanks,\nAddison")))
        ("nnimap\\+work-gmail:\\[Gmail\\]/Sent Mail"
         (gcc-self . none))
        ("nnimap\\+work-gmail:\\[Gmail\\]/Trash"
         (expiry-wait . immediate))))

;; Gmail doesn't need to save sent mail (it does this automatically)
(setq gnus-message-archive-group
      '((if (string-match "gmail\\.com" (message-sendmail-envelope-from))
            nil  ;; No need to save for Gmail
          "sent"))) ;; Archive for other accounts

(setq gnus-topic-topology 
      '(("Gnus" visible)
        (("Personal" visible)
         (("personal" visible)))
        (("Work" visible)
         (("work-gmail" visible)))))

(setq gnus-topic-alist
      '(("personal" . ("nnimap+personal:INBOX"))
        ("work-gmail" . ("nnimap+work-gmail:INBOX"
                         "nnimap+work-gmail:[Gmail]/Sent Mail"
                         "nnimap+work-gmail:[Gmail]/All Mail"))
        ("Gnus" . ("nndraft:drafts"))))

;; Open articles in a vertical split
(gnus-add-configuration
 '(article
   (horizontal 1.0
     (summary 0.5 point)
     (article 1.0))))

;; Sort by reverse number (newest first)
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)))
(setq gnus-article-sort-functions
      '((not gnus-article-sort-by-number)))

(gnus-demon-add-handler 'gnus-group-get-new-news 5 t)
(gnus-demon-init)

(setq magit-git-executable "/Users/me/.nix-profile/bin/git")

(transient-define-prefix my/inbox-menu ()
  "Transient menu for getting to my inboxes"
  ["Submenu Actions"
   ("i" "Inbox" my/open-inbox)
   ("m" "Mobile Inbox" my/open-inbox-mobile)])

(transient-define-prefix my/go-menu ()
  "Transient menu for navigating key files."
  ["Go To"
   ("i" "Inboxes" my/inbox-menu)
   ("a" "Agenda" my/org-agenda-daily-dashboard)
   ("e" "Emacs Config" my/open-emacs-config)
   ("p" "Prompts" my/open-prompts)
   ("b" "Budget" my/open-budget)
   ("l" "Log" my/open-log)])

(transient-define-prefix my/insert-menu ()
  "Transient menu for inserting stuff places (usually under the cursor)."
  ["Insert"
   ("l" "LLM Prompt" my/search-llm-prompts)])

(transient-define-prefix my/search-menu ()
  "Transient menu for searching around the buffer(s), project, and filesystem."
  ["Search"
   ("s" "Line" consult-line)
   ("b" "Buffer" consult-buffer)
   ("f" "Find" consult-find)
   ("o" "Org Heading" consult-org-heading)
   ("r" "Ripgrep" consult-ripgrep)])

(define-prefix-command 'my-custom-prefix)
(evil-define-key 'normal 'global (kbd "C-a") 'my-custom-prefix)
(which-key-add-key-based-replacements "C-a" "my commands")

;; create "go" prefix map
(define-prefix-command 'my-go-prefix)
(evil-define-key 'normal 'global (kbd "C-a g") 'my-go-prefix)
(which-key-add-key-based-replacements "C-a g" "go")

(defun my/open-compilation-file-in-other-window ()
  "open the current compilation match in another window.
  creates a new window if needed or reuses an existing one."
  (interactive)
  (let ((window-count (length (window-list))))
    (condition-case err
  (if (= window-count 1)
      ;; only one window, use built-in function that creates a new window
      (compilation-display-error)
    ;; multiple windows exist, use the next window
    (let ((this-window (selected-window)))
      (other-window 1)
      (let ((target-window (selected-window)))
	(select-window this-window)
	;; use next-error-no-select to get location without changing windows
	(let ((location (next-error-no-select)))
	  (select-window target-window)
	  (switch-to-buffer (marker-buffer (car location)))
	  (goto-char (marker-position (car location)))))))
;; catch any errors silently
(error (message "no valid location found at point")))))

(evil-define-key 'normal 'global (kbd "C-a g f") 'my/open-compilation-file-in-other-window)
(which-key-add-key-based-replacements "C-a g f" "go to file")

(evil-define-key 'normal 'global (kbd "C-a g d") 'lsp-find-definition)
(which-key-add-key-based-replacements "C-a g d" "go to definition")
(evil-define-key 'normal 'global (kbd "C-a g e") (lambda () (interactive) (find-file "/users/me/nix/system/with/user/with/program/emacs.org")))
(which-key-add-key-based-replacements "C-a g e" "emacs config")

(define-prefix-command 'my-compile-prefix)
(evil-define-key 'normal 'global (kbd "C-a c") 'my-compile-prefix)
(which-key-add-key-based-replacements "C-a c" "compile")

(define-prefix-command 'my-nix-compile-prefix)
(evil-define-key 'normal 'global (kbd "C-a c n") 'my-nix-compile-prefix)
(which-key-add-key-based-replacements "C-a c n" "nix")

(evil-define-key 'normal 'global (kbd "C-a c n r") 'my/nix-rebuild)
(which-key-add-key-based-replacements "C-a c n r" "rebuild")

(evil-define-key 'normal 'global (kbd "C-a c n f") 'my/nix-format)
(which-key-add-key-based-replacements "C-a c n f" "format")

(evil-define-key 'normal 'global (kbd "C-a c n c") 'my/nix-commit)
(which-key-add-key-based-replacements "C-a c n c" "commit")

;; create "find" prefix map
(define-prefix-command 'my-find-prefix)
(evil-define-key 'normal 'global (kbd "C-a f") 'my-find-prefix)
(which-key-add-key-based-replacements "C-a f" "find")

(evil-define-key 'normal 'global (kbd "C-a f g") 'projectile-ripgrep)
(which-key-add-key-based-replacements "C-a f g" "ripgrep")

(evil-define-key 'normal 'global (kbd "C-a f p") 'projectile-switch-project)
(which-key-add-key-based-replacements "C-a f p" "project")

(evil-define-key 'normal 'global (kbd "C-a f f") 'find-file)
(which-key-add-key-based-replacements "C-a f f" "file in directory")

(evil-define-key 'normal 'global (kbd "C-a f F") 'projectile-find-file)
(which-key-add-key-based-replacements "C-a f F" "file in project")

(evil-define-key 'normal 'global (kbd "C-a f b") 'consult-buffer)
(which-key-add-key-based-replacements "C-a f b" "find an open buffer")

(evil-define-text-object evil-inner-org-src-block (count &optional beg end type)
		   "Select an org source block, excluding the begin/end lines."
		   (when (org-in-src-block-p)
		     (save-excursion
		       (let* ((element (org-element-at-point))
			      (begin (org-element-property :begin element))
			      (end (org-element-property :end element))
			      (begin-adjusted (progn
						(goto-char begin)
						(forward-line 1)
						(point))))
			 (goto-char begin)
			 (re-search-forward "^[ \t]*#\\+end_src" end t)
			 (forward-line -1)
			 (let ((end-adjusted (line-end-position)))
			   (evil-range begin-adjusted end-adjusted 'line))))))

(evil-define-text-object evil-a-org-src-block (count &optional beg end type)
		   "Select an org source block, including the begin/end lines."
		   (when (org-in-src-block-p)
		     (save-excursion
		       (let* ((element (org-element-at-point))
			      (begin (org-element-property :begin element))
			      (end (org-element-property :end element)))
			 (evil-range begin end 'line)))))

(which-key-add-key-based-replacements "is" "inside src block")
(which-key-add-key-based-replacements "as" "around src block")

(evil-define-key 'normal 'global (kbd "U") 'undo-redo)

(defun my/org-agenda-daily-dashboard ()
  "Open the custom 'daily dashboard' org-agenda view."
  (interactive)
  (org-agenda nil "d"))

(evil-define-key 'normal 'global (kbd "D") 'kill-buffer)
(evil-define-key 'normal magit-mode-map (kbd "C-d") 'kill-buffer)

(evil-define-key 'normal 'global (kbd "C-e") 'elfeed)
(evil-define-key 'normal elfeed-search-mode-map (kbd "C-r") 'elfeed-update)

(evil-define-key 'normal 'global (kbd "<f6>") 'my/toggle-theme)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "<f6>") 'my/toggle-theme))

(evil-define-key 'normal 'global (kbd "C-z") 'magit-status)

(with-eval-after-load 'elfeed-show
  (require 'hnreader)
  (require 'evil)

  (defun my/elfeed-show-hn-comments ()
    "Open Hacker News comments for the link at point in elfeed-show-mode."
    (interactive)
    (message "my/elfeed-show-hn-comments invoked.")
    (let ((link (elfeed-get-link-at-point)))
(message "Link at point: %s" link)
;; Check if it's a valid HN item link
(if (and link (string-match "news\\.ycombinator\\.com/item\\?id=[0-9]+" link))
    (progn ;; Use progn to execute multiple forms
      (message "Found HN link: %s. Calling hnreader-comment..." link)
      ;; Pass the full link URL to hnreader-comment
      (hnreader-comment link)
      (message "hnreader-comment called with URL."))
  (message "No Hacker News item link found at point or link doesn't match pattern."))))

  (evil-define-key 'normal elfeed-show-mode-map
	     (kbd "c") #'my/elfeed-show-hn-comments))

;;(defun my/open-ripgrep-result-in-split ()
;; "Open the ripgrep result at point in a vertical split."
;;(interactive)
;;(let ((window-count (length (window-list))))
;; (when (= window-count 1)
;;  (split-window-right))
;;(other-window 1)
;;(compile-goto-error)))

;;(with-eval-after-load 'rg
;;(evil-define-key 'normal rg-mode-map (kbd "RET") 'my/open-ripgrep-result-in-split))

  ;;;(evil-define-key 'normal 'global (kbd "C-b") 'projectile-switch-to-buffer)
;;(evil-define-key 'normal 'global (kbd "C-p") 'projectile-switch-project)
;;(evil-define-key 'normal 'global (kbd "C-f") 'projectile-find-file)
;;(evil-define-key 'normal magit-mode-map (kbd "C-b") 'projectile-switch-to-buffer)
;;(evil-define-key 'normal magit-mode-map (kbd "C-p") 'projectile-switch-project)
;;(evil-define-key 'normal magit-mode-map (kbd "C-f") 'projectile-find-file)

(evil-global-set-key 'normal (kbd "C-b") 'projectile-switch-to-buffer)
(evil-global-set-key 'normal (kbd "C-p") 'projectile-switch-project)
(evil-global-set-key 'normal (kbd "C-f") 'projectile-find-file)
(evil-define-key 'normal magit-mode-map (kbd "C-b") 'projectile-switch-to-buffer)
(evil-define-key 'normal magit-mode-map (kbd "C-p") 'projectile-switch-project)
(evil-define-key 'normal magit-mode-map (kbd "C-f") 'projectile-find-file)
(evil-define-key 'normal vterm-mode-map (kbd "C-b") 'projectile-switch-to-buffer)
(evil-define-key 'normal vterm-mode-map (kbd "C-p") 'projectile-switch-project)
(evil-define-key 'normal vterm-mode-map (kbd "C-f") 'projectile-find-file)

(defun clean-notes-buffer ()
  "Clean up a notes buffer by:
    1. Deleting lines starting with '- State'
    2. Removing extra blank lines
    3. Converting '- Note taken on [DATE] \\' to '** [DATE]'
    4. Removing all indentation from the buffer"
  (interactive)
  (save-excursion
    ;; Go to beginning of buffer
    (goto-char (point-min))

    ;; Delete lines starting with "- State"
    (while (re-search-forward "^- State.*$" nil t)
(replace-match ""))

    ;; Clean up consecutive blank lines
    (goto-char (point-min))
    (while (re-search-forward "^\n\\s-*\n" nil t)
(replace-match "\n"))

    ;; Convert note lines to new format (fixed to handle backslashes properly)
    (goto-char (point-min))
    (while (re-search-forward "^- Note taken on \\(\\[[0-9-]+ [A-Za-z]+\\( [0-9:]+\\)?\\]\\)\\s-*\\\\\\\\.*$" nil t)
(replace-match "** \\1"))

    ;; Remove all indentation (leading spaces/tabs) from all lines
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+" nil t)
(replace-match "")))
  (message "Notes cleanup completed!"))

(defun my/add-tag-to-headings-in-region (tag)
  "Add TAG to all org headings in the selected region."
  (interactive "sTag to add: ")
  (save-excursion
    (let ((end-marker (copy-marker (region-end)))
    (pos (region-beginning)))
(goto-char pos)
(while (and (< (point) end-marker)
	    (re-search-forward "^\\*+ " end-marker t))
  (org-set-tags (cons tag (org-get-tags)))
  (outline-next-heading)))))

(defun my/projectile-find-file-in-all-projects ()
  "Find file across all registered Projectile projects with improved performance."
  (interactive)
  (let* ((projects (projectile-relevant-known-projects))
   (file-cache-var 'my/projectile-all-files-cache)
   (cache-validity-seconds 300) ;; 5 minute cache validity
   (current-time (current-time))
   (use-cache (and (boundp file-cache-var)
		   (< (float-time (time-subtract 
				   current-time
				   (get file-cache-var 'timestamp)))
		      cache-validity-seconds)))
   (cached-files (and use-cache (symbol-value file-cache-var))))

    (if use-cache
  (message "Using cached file list (%d files)" (length cached-files))
;; Build cache using external commands for speed
(message "Building file list from %d projects..." (length projects))
(let ((all-files '())
      (temp-file (make-temp-file "projectile-files-")))
  ;; Using external find/sort is much faster than pure elisp
  (with-temp-file temp-file
    (dolist (project projects)
      (when (file-exists-p project)
	(let* ((project-name (file-name-nondirectory 
			      (directory-file-name project)))
	       ;; Add project name prefix to each file for context
	       (cmd (format "cd %s && find . -type f -not -path \"*/\\.*\" | sort | sed 's|^\\.|%s:|'"
			    (shell-quote-argument project)
			    project-name)))
	  (call-process-shell-command cmd nil t)))))

  ;; Read results back and build alist of (display . filepath)
  (with-temp-buffer
    (insert-file-contents temp-file)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
	     ;; Fix: Only split on the first colon
	     (split-pos (string-match ":" line))
	     (project-name (when split-pos (substring line 0 split-pos)))
	     (rel-file (when split-pos (substring line (1+ split-pos))))
	     (full-path (when (and project-name rel-file)
			  (expand-file-name
			   (string-remove-prefix "./" rel-file)
			   (car (seq-filter (lambda (p) 
					      (string-suffix-p project-name p))
					    projects))))))
	(when (and project-name rel-file full-path)
	  (push (cons (concat project-name ":" rel-file) full-path) all-files)))
      (forward-line 1)))

  (delete-file temp-file)
  ;; Save and timestamp the cache
  (set file-cache-var all-files)
  (put file-cache-var 'timestamp current-time)
  (message "Found %d files across projects" (length all-files))))

    ;; Use the cached or newly-built list
    (let ((file-list (if use-cache cached-files (symbol-value file-cache-var))))
(if file-list
    ;; Use completing-read for the selection interface
    (let* ((chosen (completing-read "Find file in projects: " 
				    (mapcar #'car file-list) nil t))
	   (file-path (cdr (assoc chosen file-list))))
      (when file-path
	(find-file file-path)))
  (message "No files found across projects")))))

(defun my/wikipedia-search-to-org (search-term)
  "Search Wikipedia for SEARCH-TERM, fetch the first result,
  and display its content converted to Org format in a new buffer.
  Requires `pandoc` to be installed."
  (interactive "sSearch Wikipedia for: ")
  (unless (executable-find "pandoc")
    (error "pandoc executable not found. Please install pandoc."))

  (require 'json)
  (require 'url)

  (let* ((encoded-search-term (url-hexify-string search-term))
   (search-api-url (format "https://en.wikipedia.org/w/api.php?action=opensearch&search=%s&limit=1&namespace=0&format=json"
			   encoded-search-term))
   search-json-string search-data page-title page-url html-content org-output org-buffer-name)

    (with-current-buffer (url-retrieve-synchronously search-api-url)
(goto-char (point-min))
(unless (re-search-forward "\n\n" nil t) ; Skip HTTP headers
  (kill-buffer (current-buffer))
  (error "Could not find HTTP headers in Wikipedia API response"))
(setq search-json-string (buffer-substring-no-properties (point) (point-max)))
(kill-buffer (current-buffer)))

    (unless (and search-json-string (not (string-empty-p search-json-string)))
(error "Failed to fetch or empty search results from Wikipedia API for: %s" search-term))

    (condition-case err
  ;; Explicitly use :array-type 'list to ensure lists are returned
  (setq search-data (json-parse-string search-json-string :array-type 'list))
(error (error "Failed to parse JSON: %s. JSON was: %s" (error-message-string err) search-json-string)))

    ;; Now the checks should work as expected with lists
    (unless (and (listp search-data)
	   (>= (length search-data) 4)
	   (listp (nth 1 search-data)) (consp (nth 1 search-data)) ; Ensure titles list is non-empty
	   (stringp (car (nth 1 search-data))) ; Ensure first title is a string
	   (listp (nth 3 search-data)) (consp (nth 3 search-data)) ; Ensure URLs list is non-empty
	   (stringp (car (nth 3 search-data)))) ; Ensure first URL is a string
(error "Unexpected JSON structure, empty result, or non-string title/URL from Wikipedia API. Data: %S" search-data))

    (setq page-title (car (nth 1 search-data)))
    (setq page-url (car (nth 3 search-data)))

    (with-current-buffer (url-retrieve-synchronously page-url)
(goto-char (point-min))
(unless (re-search-forward "\n\n" nil t) ; Skip HTTP headers
  (kill-buffer (current-buffer))
  (error "Could not find HTTP headers in Wikipedia page response for %s" page-url))
(setq html-content (buffer-substring-no-properties (point) (point-max)))
(kill-buffer (current-buffer)))

    (unless (and html-content (not (string-empty-p html-content)))
(error "Failed to fetch or empty page content from URL: %s" page-url))

    (setq org-output
    (with-temp-buffer
      (insert html-content)
      (shell-command-on-region (point-min) (point-max)
			       "pandoc -f html -t org --wrap=none"
			       (current-buffer) t) ; t to replace
      (buffer-string)))

    (if (or (null org-output) (string-empty-p org-output))
  (error "Pandoc conversion resulted in empty output for %s" page-title)
(setq org-buffer-name (format "*Wikipedia: %s (Org)*" page-title))
(with-current-buffer (get-buffer-create org-buffer-name)
  (erase-buffer)
  (insert org-output)
  (when (fboundp 'org-mode) (org-mode))
  (goto-char (point-min)))
(switch-to-buffer-other-window org-buffer-name)
(message "Wikipedia page '%s' rendered as Org." page-title))))

(defun my/org-refile-to-new-file ()
  "Refile current heading to a new file named after the heading."
  (interactive)
  (let* ((heading (nth 4 (org-heading-components)))
   (safe-name (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" heading)))
   (new-file (concat (file-name-as-directory org-directory) safe-name ".org")))
    (when (y-or-n-p (format "Create and refile to %s? " new-file))
(with-temp-buffer
  (write-file new-file))
(org-refile nil nil (list heading new-file nil nil)))))

(defun my/open-pr-url-at-point ()
  "Open the PR_URL property of the current org agenda item."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
	       (org-agenda-error)))
   (buffer (marker-buffer marker))
   (pos (marker-position marker))
   url)
    (with-current-buffer buffer
(save-excursion
  (goto-char pos)
  (setq url (org-entry-get (point) "PR_URL"))))
    (when url
(browse-url url))))

;; Bind it to a key in org-agenda-mode-map
(define-key org-agenda-mode-map (kbd "C-c u") 'open-pr-url-at-point)

(evil-define-key 'normal 'global (kbd "C-g") #'my/go-menu)
(evil-define-key 'normal 'global (kbd "C-i") #'my/insert-menu)
(evil-define-key 'normal 'global (kbd "C-s") #'my/search-menu)
(evil-define-key 'normal 'global (kbd "C-l") 'gptel-menu)

(defun my/org-mode-set-keybindings ()
  (interactive)
  "Sets all my org mode specific keybindings"
  (evil-define-key 'operator org-mode-map (kbd "is") 'evil-inner-org-src-block)
  (evil-define-key 'operator org-mode-map (kbd "as") 'evil-a-org-src-block)
  (evil-define-key 'normal org-mode-map (kbd "C-c C-h") 'consult-org-heading))

(defun my/org-agenda-mode-set-keybindings ()
  (interactive)
  "Sets all my org mode specific keybindings"
  (define-key org-agenda-mode-map (kbd "C-g") #'my/go-menu)
  (define-key org-agenda-mode-map (kbd "C-i") #'my/insert-menu)
  (define-key org-agenda-mode-map (kbd "C-s") #'my/search-menu)
  (define-key org-agenda-mode-map (kbd "C-c C-h") 'consult-org-agenda))

(defun my/magit-status-mode-set-keybindings ()
  (interactive)
  "Sets all my org mode specific keybindings"
  (define-key org-agenda-mode-map (kbd "C-g") #'my/go-menu)
  (define-key org-agenda-mode-map (kbd "C-i") #'my/insert-menu)
  (define-key org-agenda-mode-map (kbd "C-s") #'my/search-menu)
  (define-key org-agenda-mode-map (kbd "C-c C-h") 'consult-org-agenda))

(defun org-mode-init ()
  "Function to run on org mode init"
  (org-display-inline-images)
  (variable-pitch-mode)
  (breadcrumb-local-mode)
  (org-indent-mode)
  (org-modern-mode)
  (setq-local line-spacing 0.5)
  (my/toggle-olivetti)
  (my/org-mode-set-keybindings))
(add-hook 'org-mode-hook #'org-mode-init)

(defun org-agenda-mode-init ()
  "Function to run on org agenda mode init"
  (variable-pitch-mode)
  (my/org-agenda-mode-set-keybindings)
  (my/toggle-olivetti))
(add-hook 'org-agenda-mode-hook #'org-agenda-mode-init)

(defun magit-status-mode-init ()
  "Function to run on magit status mode init"
  (my/magit-status-mode-set-keybindings)
  (my/toggle-olivetti))
(add-hook 'magit-status-mode-hook #'magit-status-mode-init)

(advice-add 'my/toggle-theme :after 'my/update-olivetti-fringe-face)

(defun my/reload-config ()
  "Tangle emacs.org and reload the resulting init.el."
  (interactive)
  (when (file-exists-p "~/nix/system/with/user/with/program/emacs.org")
    (let ((org-file "~/nix/system/with/user/with/program/emacs.org")
          (init-file "~/nix/system/with/user/with/program/init.el"))
      (with-temp-buffer
        (shell-command (format "cd %s && emacs --batch -l org %s --eval '(org-babel-tangle)'" 
                              (file-name-directory org-file)
                              (file-name-nondirectory org-file)) 
                     (current-buffer))
        (message "%s" (buffer-string)))
      (load-file init-file)
      (message "Configuration reloaded successfully!"))))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
		      ;;; init.el ends here
