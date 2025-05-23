;; init.el --- My personal Emacs configuration  -*- lexical-binding: t -*-

;; Commentary:
;; This is my personal Emacs configuration file.
;; It sets up various packages and configurations for development work.

;; Code:

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
(setq-default mode-line-format nil)
(advice-add #'display-startup-echo-area-message :override #'ignore)

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

(defun kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc 'kill-buffer
  (delq (current-buffer)
	(buffer-list))))

(defun my/delete-this-file ()
  "Delete the current file and kill its buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file
	 (y-or-n-p (format "Delete %s?" file)))
(delete-file file)
(kill-buffer)
(message "Deleted %s" file))))

(require 'package)

;; Use Package Configuration
(use-package nerd-icons)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  dashboard-set-navigator t)
  (setq dashboard-heading-icons '((recents   . "nf-oct-history")
			    (bookmarks . "nf-oct-bookmark")
			    (projects  . "nf-oct-project"))))

(set-face-attribute 'default nil :family "Iosevka" :height 140)
(set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 100)

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
	     (kbd "<left>") 'org-agenda-earlier
	     (kbd "<right>") 'org-agenda-later
	     (kbd "C-c j") 'org-agenda-goto-date
	     (kbd "gx")  'org-agenda-open-link
	     (kbd "t") 'org-agenda-todo
	     (kbd "T") 'org-agenda-todo-yesterday)

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  ;; Specify known projects
  (setq projectile-known-projects
  (mapcar 'expand-file-name
	  '("~/notes"
	    "~/nix"
	    "~/bitwarden/clients"
	    "~/bitwarden/server"
	    "~/bitwarden/sdk"
	    "~/bitwarden/sdk-internal"
	    "~/bitwarden/wg-open-source-at-bitwarden"
	    "~/bitwarden/contributing-docs"
	    "~/d"
	    "~/binwarden"
	    "~/recipes")))
  ;; Disable auto-discovery
  (setq projectile-auto-discover nil)
  ;; Save the project list immediately
  (projectile-save-known-projects)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("C-c b" . consult-buffer)
   ("C-c f" . consult-find)
   ("C-c r" . consult-ripgrep)))

;; Custom find-from-here function
(defun find-from-here ()
  "Find files from current buffer's directory."
  (interactive)
  (when buffer-file-name
    (consult-find (file-name-directory buffer-file-name))))

(global-set-key (kbd "C-c d") 'find-from-here)

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode . lsp)
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

;; Basic org settings
(use-package org
  :ensure t
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-directory "~/notes")
  (setq org-startup-truncated nil)
  (setq org-agenda-files (list org-directory))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-global-properties
  '(("STATUS_ALL" . "Not-Started\\|In-Progress\\|Blocked\\|Done")
    ("TYPE_ALL" . "Bug\\|Feature\\|Chore\\|Spike\\|Review")))
  (setq org-clock-persist 'history
  org-clock-idle-time 15
  org-clock-into-drawer t)
  (org-clock-persistence-insinuate))

(defun sanitize-filename (name)
  "Sanitize a filename NAME."
  (downcase (replace-regexp-in-string "[^a-zA-Z0-9]" "-" name)))

(setq org-capture-templates
'(("p" "Personal habit" entry
   (file (lambda ()
	   (let ((name (read-string "File name: ")))
	     (expand-file-name (concat (sanitize-filename name) ".org")
			       "~/notes/"))))
   "* TODO %^{Task description}\nSCHEDULED: <%<%Y-%m-%d> +1d>\n:PROPERTIES:\n:CATEGORIES: personal habit\n:CUSTOM_ID: %^{Custom id}\n:END:")

  ("f" "Family habit" entry
   (file (lambda ()
	   (let ((name (read-string "File name: ")))
	     (expand-file-name (concat (sanitize-filename name) ".org")
			       "~/notes/"))))
   "* TODO %^{Task description}\nSCHEDULED: <%<%Y-%m-%d> +1d>\n:PROPERTIES:\n:CATEGORIES: family habit\n:CUSTOM_ID: %^{Custom id}\n:END:")

  ("w" "Work habit" entry
   (file (lambda ()
	   (let ((name (read-string "File name: ")))
	     (expand-file-name (concat (sanitize-filename name) ".org")
			       "~/notes/"))))
   "* TODO %^{Task description}\nSCHEDULED: <%<%Y-%m-%d> +1d>\n:PROPERTIES:\n:CATEGORIES: work habit\n:CUSTOM_ID: %^{Custom id}\n:END:")

  ("j" "Journal Entry" plain
   (function (lambda ()
	       (let* ((id (completing-read "Choose entry: "
					   '("me" "emily" "lincoln" "nora" "fern" "harry")))
		      (file "~/notes/log.org")
		      (full-id (concat "log-" id)))
		 (find-file file)
		 (goto-char (point-min))
		 (when (re-search-forward (format ":CUSTOM_ID: %s" full-id) nil t)
		   (org-back-to-heading t)
		   (re-search-forward ":LOGBOOK:" nil t)
		   (forward-line 1)))))
   "- Note taken on %U \\\\\n  %?"
   :immediate-finish nil)

  ("e" "Event" entry
   (file "~/notes/events.org")
   "* %^{Description}\nSCHEDULED: %^T\n:PROPERTIES:\n:CUSTOM_ID: %^{ID}\n:CATEGORIES: %^{Category|personal habit|family habit|work habit|one-off|event|school-function|holiday|birthday|work meeting}\n:END:\n\n  %?"
   :immediate-finish nil)))

;; Face customization
(with-eval-after-load 'org
  (set-face-attribute 'org-scheduled-previously nil
		:foreground "#d79921"
		:weight 'bold))

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

;; Global agenda settings
(setq org-agenda-block-separator nil)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-timegrid-use-ampm t)
(setq org-agenda-time-leading-zero t)
(setq org-agenda-todo-keyword-format "%s")
(setq org-agenda-include-diary t)
(setq org-refile-targets '((nil :maxlevel . 8)
		     (org-agenda-files :maxlevel . 2)))

(require 'diary-lib)

;; Super Agenda Configuration
(use-package org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-header-map nil)  ; Disable super-agenda keybindings
  (setq org-super-agenda-header-properties nil)
  (org-super-agenda-mode))

(setq warning-suppress-types '((org-element)))

;; Custom agenda commands
(setq org-agenda-custom-commands
'(("d" "daily dashboard"
   ((agenda "Schedule and Habits"
	    ((org-agenda-span 'day)
	     (org-agenda-sorting-strategy '((agenda time-up todo-state-down alpha-up)))
	     (org-agenda-overriding-header "")
	     (org-super-agenda-groups
	      '((:name "Today's Schedule"
		       :time-grid t)
		(:name "Events Today"
		       :property ("CATEGORIES" (lambda (value)
						 (message "Checking events: %s" value)
						 (and value
						      (string-match-p "event" value)))))
		(:name "Inbox items"
		       :property ("CATEGORIES" (lambda (value)
						 (message "Checking inbox: %s" value)
						 (and value
						      (string-match-p "inbox" value)))))
		(:name "Tasks"
		       :property ("CATEGORIES" (lambda (value)
						 (message "Checking tasks: %s" value)
						 (and value
						      (string-match-p "task" value)))))
		(:name "Code reviews"
		       :property ("CATEGORIES" (lambda (value)
						 (message "Checking code reviews: %s" value)
						 (and value
						      (string-match-p "code-review" value)))))
		(:name "Personal Habits"
		       :property ("CATEGORIES" (lambda (value)
						 (message "Checking personal habits: %s" value)
						 (and value
						      (string-match-p "habit" value)
						      (string-match-p "personal" value)))))
		(:name "Family Habits"
		       :property ("CATEGORIES" (lambda (value)
						 (message "Checking family habits: %s" value)
						 (and value
						      (string-match-p "habit" value)
						      (string-match-p "family" value)))))
		(:name "Work Habits"
		       :property ("CATEGORIES" (lambda (value)
						 (message "Checking work habits: %s" value)
						 (and value
						      (string-match-p "habit" value)
						      (string-match-p "work" value)))))
		(:discard (:anything t))))))))))

;; Agenda refresh function
(defun refresh-org-agenda ()
  "Refresh org agenda files and rebuild agenda view."
  (interactive)
  (setq org-agenda-files (list org-directory))
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
(org-agenda-redo t))))

(global-set-key (kbd "C-c r") 'refresh-org-agenda)

;; Agenda appearance settings
(setq org-agenda-time-grid-use-ampm t)
(setq org-agenda-with-times t)
(setq org-agenda-time-format "%I:%M%p")
(setq org-agenda-prefix-format
'((agenda . " ○ %t ")
  (tags   . "○ ")
  (todo   . "○ ")))

;; Auto-save settings for org files
(defun my-org-auto-save-settings ()
  (setq-local auto-save-interval 1)
  (setq-local auto-save-timeout 5))

(add-hook 'org-mode-hook 'my-org-auto-save-settings)

;; Face customizations for org mode
(custom-set-faces
 '(org-document-info-keyword ((t (:height 1.0))))
 '(org-document-title ((t (:height 140))))
 '(org-level-1 ((t (:height 140))))
 '(org-level-2 ((t (:height 140))))
 '(org-level-3 ((t (:height 140))))
 '(org-level-4 ((t (:height 140))))
 '(org-level-5 ((t (:height 140))))
 '(org-level-6 ((t (:height 140))))
 '(org-level-7 ((t (:height 140))))
 '(org-level-8 ((t (:height 140))))
 '(org-modern-label ((t (:height 140))))
 '(org-modern-statistics ((t (:height 140))))
 '(org-modern-tag ((t (:height 140))))
 '(org-drawer ((t (:height 140))))
 '(org-drawer-content ((t (:height 140))))
 '(variable-pitch-text ((t (:height 140))))
 '(variable-pitch ((t (:height 140))))
 '(org-property-value ((t (:height 140))))
 '(org-special-keyword ((t (:height 140)))))

(require 'ghub)

(defvar my/github-pr-file "~/notes/github-prs.org"
  "File to store GitHub PR todos.")

(defvar my/github-pr-queries
  '(("Involved PRs" . "is:open is:pr involves:addisonbeck -author:addisonbeck")
    ("Renovate PRs" . "is:open is:pr involves:addisonbeck author:app/renovate")))

(defun my/pr-exists-p (url)
  "Check if PR with URL already exists in the org file."
  (message "Checking for existing PR: %s" url)
  (when (file-exists-p my/github-pr-file)
    (message "File exists, checking content")
    (with-temp-buffer
(insert-file-contents my/github-pr-file)
(message "File contents loaded")
;; Instead of using buffer positions, just check if the string exists
(string-match-p (regexp-quote url) (buffer-string)))))

(defun my/fetch-github-prs ()
  "Fetch PRs and create new org entries if they don't exist."
  (interactive)
  (message "Starting PR fetch")
  (let ((buf (find-file-noselect my/github-pr-file)))
    (message "Buffer created: %S" buf)
    (with-current-buffer buf
(message "In buffer")
(org-mode)
(message "Org mode enabled")
(let ((max-point (point-max)))
  (message "Max point: %S" max-point)
  (goto-char max-point)
  (message "Moved to end of buffer")
  (dolist (query-pair my/github-pr-queries)
    (let* ((section-name (car query-pair))
	   (query (cdr query-pair)))
      (message "Processing query: %s" section-name)
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
	(message "Got GraphQL response")
	(when-let ((prs (alist-get 'nodes (alist-get 'search (alist-get 'data response)))))
	  (message "Found %d PRs" (length prs))
	  (dolist (pr prs)
	    (message "Processing PR: %S" pr)
	    (let-alist pr
	      (message "Checking if PR exists: %s" .url)
	      (let ((exists-result (my/pr-exists-p .url)))
		(message "PR exists check returned: %S" exists-result)
		(unless exists-result
		  (message "PR doesn't exist, inserting")
		  (let ((insert-point (point)))
		    (message "Current point before insert: %S" insert-point)
		    (insert (format "* TODO %s
:PROPERTIES:
:PR_URL: %s
:REPO: %s
:AUTHOR: %s
:CATEGORIES: code-review
:END:

[[%s][Open in GitHub]]

"
				    .title
				    .url
				    .repository.nameWithOwner
				    .author.login
				    .url))
		    (message "Insert completed"))))))))))))
    (message "Saving buffer")
    (save-buffer)
    (message "PR fetch completed")))

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
  (load-theme 'gruvbox-dark-hard t))

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

  (setq elfeed-search-filter "+unread or +starred")
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
  (elfeed-protocol-feeds '(("fever+https://me@rss.addisonbeck.dev"
		      :api-url "https://rss.addisonbeck.dev/api/fever.php"
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

(defun my/reload-config ()
  "Reload Emacs configuration by tangling and loading init.org."
  (let ((init-org "~/nix/system/with/user/with/program/init.org")
        (temp-el "/tmp/init-temp.el"))
    (with-current-buffer (find-file-noselect init-org)
      ;; Tangle only emacs-lisp blocks to our temp file
      (org-babel-tangle-file init-org temp-el "emacs-lisp")
      ;; Load the tangled config
      (load temp-el)
      ;; Clean up
      (delete-file temp-el)
      "Configuration reloaded successfully")))

(use-package gptel
  :ensure t
  :config
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
    (message "Cleared token, making request...")
    ;; Make request that should trigger token refresh
    (gptel-request
     "Test message"
     :callback (lambda (response info)
	   (message "=== Request completed ===")
	   (message "New token (first 50 chars): %s..."
		    (substring gptel-copilot--exchanged-token 0 50))
	   (message "Response status: %s" (plist-get info :status))
	   (message "Got response: %s" response))))

  (setq gptel-backend gptel-copilot-backend
  ;;gptel-model 'gpt-4o-2024-08-06
  gptel-model ' claude-3.5-sonnet
  gptel-auto-save-directory "~/chats"
  gptel--mark-prompts-and-responses nil
  gptel-auto-save-buffers t
  gptel-prompt-prefix
  "You are a large language model living in Emacs and a helpful assistant.
			 You are assisting a software engineer at Bitwarden, an open source password management solution.
			 When expressing uncertainty, make it clear.
			 When making assumptions, state them explicitly.
			 Always respond concisely."
  gptel-default-mode 'markdown-mode))

(setq gptel-use-tools t
gptel-tools nil)  

(defun register-gptel-tool (tool-name)
  "Register a tool with gptel by its NAME."
  (add-to-list 'gptel-tools (gptel-get-tool tool-name)))

(gptel-make-tool
 :name "create_gptel_tool"
 :function (lambda (name description function args category)
             (message "Running create_gptel_tool with name: %s" name)
             (message "Debug: Starting tool creation/update process")
             (let* ((config-file "~/nix/system/with/user/with/program/init.org")
                    (tool-template (format "
** %s

#+begin_src emacs-lisp
    (gptel-make-tool
     :name \"%s\"
     :function %s
     :description \"%s\"
     :args '%s
     :category \"%s\")

    (register-gptel-tool \"%s\")
#+end_src
" 
                                         (capitalize name)
                                         name
                                         function
                                         description
                                         args
                                         category
                                         name))
                    (success nil))

               (with-current-buffer (find-file-noselect config-file)
                 (goto-char (point-min))
                 (when (search-forward "* GPTel Tools" nil t)
                   (message "Debug: Found GPTel Tools section")
                   (let ((tools-section-start (point))
                         (tools-section-end (save-excursion
                                            (if (re-search-forward "^\\* " nil t)
                                                (line-beginning-position)
                                              (point-max))))
                         (found (save-excursion
                                (re-search-forward (format "^** %s$" (capitalize name)) nil t))))
                     (message "Debug: Tool search result: %s" found)
                     (if found
                         (progn
                           (goto-char found)
                           (let ((begin found)
                                 (end (save-excursion
                                       (if (re-search-forward "^\\*\\* \\|^\\* " nil t)
                                           (point)
                                         (point-max)))))
                             (delete-region begin end)
                             (goto-char begin)
                             (insert tool-template)
                             (setq success 'updated)))
                       ;; For new tools, find the last tool section
                       (goto-char tools-section-start)
                       (let ((last-tool-pos tools-section-start))
                         (while (re-search-forward "^\\*\\* " tools-section-end t)
                           (setq last-tool-pos (point)))
                         (goto-char last-tool-pos)
                         ;; Move to end of this tool section
                         (if (re-search-forward "^\\*\\* \\|^\\* " tools-section-end t)
                             (goto-char (match-beginning 0))
                           (goto-char tools-section-end))
                         (insert "\n" tool-template)
                         (setq success 'created))))
                   (save-buffer)))

               (pcase success
                 ('updated (format "Successfully updated existing tool '%s' in %s" name config-file))
                 ('created (format "Successfully created new tool '%s' in %s" name config-file))
                 (_ (format "Failed to create/update tool '%s'. Could not find GPTel Tools section in config." name)))))
 :description "Creates or updates a GPTel tool in the Emacs configuration"
 :args '((:name "name"
          :type string
          :description "name of the tool to create")
         (:name "description"
          :type string
          :description "description of what the tool does")
         (:name "function"
          :type string
          :description "elisp function implementation as a string")
         (:name "args"
          :type string
          :description "list of argument specifications in elisp format")
         (:name "category"
          :type string
          :description "category for the tool (e.g., 'web', 'file', etc.)"))
 :category "meta")

    (register-gptel-tool "create_gptel_tool")

(gptel-make-tool
   :name "fetch_webpage"
   :function (lambda (url)
              (message "Fetching URL: %s" url)
              (let ((buffer (url-retrieve-synchronously url t nil 30)))
                (when buffer
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (re-search-forward "^$" nil t) ; Skip headers
                    (forward-char)
                    ;; Basic HTML cleanup: Convert to plain text
                    (require 'shr)
                    (let* ((dom (libxml-parse-html-region (point) (point-max)))
                           (text-buffer (generate-new-buffer " *temp*")))
                      (with-current-buffer text-buffer
                        (shr-insert-document dom)
                        ;; Clean up the text and ensure it's JSON-safe
                        (let ((content (replace-regexp-in-string 
                                      "[\u0000-\u001F\u007F]+" " "
                                      (buffer-substring-no-properties (point-min) (point-max)))))
                          (kill-buffer text-buffer)
                          (kill-buffer buffer)
                          ;; Ensure we return a proper JSON string
                          content)))))))
   :description "fetch the contents of a webpage given its url"
   :args '((:name "url"
            :type string
            :description "url of the webpage to fetch"))
   :category "web")

(register-gptel-tool "fetch_webpage")

(gptel-make-tool
 :name "create_file"
 :function (lambda (path content)
       (let ((dir (file-name-directory path)))
	 (condition-case err
	     (cond
	      ((file-exists-p path)
	       (error "File already exists: %s" path))
	      (t
	       (when dir
		 (make-directory dir t))
	       (write-region content nil path)
	       (format "Successfully created file: %s" path)))
	   (error
	    (format "Error creating file: %s" (error-message-string err))))))
 :description "Creates a new file with specified content, creating any necessary parent directories. Will not overwrite existing files."
 :args '((:name "path"
	  :type string
	  :description "path to the file to create")
   (:name "content"
	  :type string
	  :description "content to write to the file"))
 :category "file")

(register-gptel-tool "create_file")

(gptel-make-tool
 :name "read_gptel_tools_section"
 :function (lambda (dummy)
            (let ((file-path "~/nix/system/with/user/with/program/init.org"))
              (condition-case err
                  (if (file-exists-p file-path)
                      (with-temp-buffer
                        (insert-file-contents file-path)
                        (org-mode)
                        (goto-char (point-min))
                        (if (re-search-forward "^\\* GPTel Tools" nil t)
                            (let* ((section-start (line-beginning-position))
                                  (section-end (save-excursion
                                               (or (re-search-forward "^\\* " nil t)
                                                   (point-max))))
                                  (content (buffer-substring-no-properties 
                                          section-start section-end)))
                              content)
                          "GPTel Tools section not found in init.org"))
                    "File not found: ~/nix/system/with/user/with/program/init.org")
                (error
                 (format "Error reading tools section: %s" 
                        (error-message-string err))))))
 :description "Reads the GPTel Tools section from init.org to provide context about available tools"
 :args '((:name "dummy"
          :type string
          :description "dummy argument"))
 :category "file")

(register-gptel-tool "read_gptel_tools_section")

(gptel-make-tool
   :name "reload_config"
   :function (lambda (dummy)
(my/reload-config))
   :description "Reloads Emacs configuration by tangling and loading init.org"
   :args '((:name "dummy"
        :type string
        :description "dummy argument"))
   :category "emacs")

  (register-gptel-tool "reload_config")
