{
  pkgs,
  lib,
  ...
}: let
  tagAgendaBlocks = [
    {
      team = "Platform";
      type = "Feature";
    }
    {
      team = "Platform";
      type = "Bug";
    }
    {
      team = "Platform";
      type = "Chore";
    }
    {
      team = "Platform";
      type = "Spike";
    }
    {
      team = "Platform";
      type = "Review";
    }
    {team = "Myself";}
  ];

  mkTagAgendaBlock = {
    team ? "Platform",
    type ? null,
    title ? null,
  }: let
    typeFilter =
      if type != null
      then "+TYPE={${type}}"
      else "";
    blockTitle =
      if title != null
      then title
      else if type != null
      then "*${team} ${type}s*"
      else "*${team} Work*";
  in ''
    (tags-todo "TEAM={${team}}+STATUS<>{Done}${typeFilter}"
    ((org-agenda-overriding-header "\n${blockTitle}\n")
    (org-agenda-keep-with-parent t)))
  '';

  mkTagAgendaBlocks = blocks:
    builtins.concatStringsSep "\n" (map mkTagAgendaBlock blocks);

  eventCategories = [
    "personal habit"
    "work habit"
    "family habit"
    "one-off"
    "event"
    "school-function"
    "holiday"
    "birthday"
    "work meeting"
  ];

  categoriesString = builtins.concatStringsSep "|" eventCategories;

  basicConfig =
    #lisp
    ''
          ;; Set up persistent undo history and backup files in separate directories
          (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
                undo-tree-auto-save-history t
                undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

          ;; Configure indentation behavior
          ;; - Use spaces for indentation instead of tabs
          ;; - Automatically indent new lines
          (setq-default indent-tabs-mode nil)
          (electric-indent-mode 1)

          ;; Set tab display width and stops
          ;; - Tabs appear 8 spaces wide (display only)
          ;; - Create tab stops every 2 spaces up to 120
          (setq-default tab-width 2
                        indent-tabs-mode nil
                        tab-stop-list (number-sequence 2 120 2))

          ;; Disable exit confirmation prompt
          (setq confirm-kill-emacs nil)

          ;; Disable automatic backup and lock files
          (setq auto-save-default nil
                create-lockfiles nil)

          ;; Disable line wrapping
          ;;(setq-default truncate-lines t)

          ;; Enable system clipboard integration
          (setq select-enable-clipboard t)

          ;; Allow saving modified buffers without confirmation
          ;; (setq-default buffer-save-without-query t)

          ;; Enable case-insensitive search
          (setq case-fold-search t)

          ;; Enable mouse support in terminal
          (xterm-mouse-mode 1)

          ;;; Configure spell checking
          ;;; - Use aspell as the spell program
          ;;; - Set American English as dictionary
          ;;; - Enable spellcheck in text modes
          ;;; - Enable code-aware spellcheck in programming modes
          ;(setq ispell-program-name "aspell"
          ;      ispell-dictionary "american")
          ;(add-hook 'text-mode-hook 'flyspell-mode)
          ;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

          ;; Set maximum line length for text formatting
          (setq-default fill-column 77)

          ;; Disable current line highlighting
          (global-hl-line-mode -1)

        (defun kill-other-buffers ()
          "Kill all buffers except the current one"
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

    '';

  dashboardConfig =
    #lisp
    ''
      (use-package nerd-icons)
      (require 'projectile)  ;; Ensure projectile is loaded first
      (use-package dashboard
        :ensure t
        :init
        (setq dashboard-icon-type 'nerd-icons)
        (setq dashboard-projects-backend 'projectile)  ;; Explicitly set projectile as backend
        :config
        (dashboard-setup-startup-hook)
        (setq dashboard-center-content t
              ;; dashboard-startup-banner 'logo
              dashboard-items '((recents . 5)
                              (projects . 5)
                              (bookmarks . 5))
              ;; dashboard-banner-logo-title "Welcome to Emacs"
              dashboard-set-heading-icons t
              dashboard-set-file-icons t
              dashboard-show-shortcuts t  ;; Changed from dashboard-startupify-list
              dashboard-set-navigator t)
        ;; Set specific icons for dashboard
        (setq dashboard-heading-icons '((recents   . "nf-oct-history")
                                      (bookmarks . "nf-oct-bookmark")
                                      (projects  . "nf-oct-project"))))
    '';

  fontConfig =
    #lisp
    ''
      (set-face-attribute 'default nil :family "Iosevka" :height 140)
      (set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 100)
    '';

  uiConfig =
    #lisp
    ''
              ;;(background-color . "#282828")
              ;;(foreground-color . "#ebdbb2")
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
    '';

  evilConfig =
    #lisp
    ''
        ;; Set evil options before loading evil
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)  ;; Moved before evil loading
        (setq evil-want-C-u-scroll t)

        (use-package evil
          :ensure t
          :config
          (evil-mode 1)
          ;; Define functions to move cursor half page
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

          ;; Bind J and K to the new functions
          (define-key evil-normal-state-map (kbd "J") 'evil-move-half-page-down)
          (define-key evil-normal-state-map (kbd "K") 'evil-move-half-page-up))

        (use-package evil-collection
          :ensure t
          :after evil
          :config
          (evil-collection-init))

          (with-eval-after-load 'evil-collection-magit
                                (evil-collection-define-key 'normal 'magit-status-mode-map
                                "V" #'magit-start-region-select))

        (use-package evil-org
        :ensure t
        :after org
        :hook (org-mode . evil-org-mode)
        :config
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys)
        ;; Add your custom keys AFTER evil-org-agenda-set-keys
        (evil-define-key 'motion org-agenda-mode-map
            (kbd "[") 'org-agenda-earlier
            (kbd "]") 'org-agenda-later
            (kbd "C-c j") 'org-agenda-goto-date
            (kbd "gx")  'org-agenda-open-link
        ))
    '';

  serverConfig =
    #lisp
    ''
      (require 'server)
      (defvar with-editor-emacsclient-executable "${pkgs.emacs}/bin/emacsclient")  ;; Declare variable before use
      (unless (server-running-p)
        (server-start))
    '';

  projectileConfig =
    #lisp
    ''
      (use-package projectile
        :ensure t
        :config
        (projectile-mode +1)
        ;; Manually specify known projects with expanded paths
        (setq projectile-known-projects
              (mapcar 'expand-file-name
                      '("~/notes"
                        "~/nix"
                        "~/bitwarden/clients"
                        "~/bitwarden/server"
                        "~/bitwarden/sdk"
                        "~/bitwarden/sdk-interanl"
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
    '';

  magitConfig =
    #lisp
    ''
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
    '';

  treesitterConfig =
    #lisp
    ''
      ;; Tree-sitter configuration
      (require 'treesit)

      ;; Enable tree-sitter based modes by default
      (setq treesit-language-source-alist nil) ;; we're using nix-provided parsers

      ;; Configure nix-mode to use tree-sitter
      (require 'nix-mode)
      (defvar nix-mode-use-tree-sitter t)  ;; Declare variable before use
    '';

  verticoConfig =
    #lisp
    ''
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

      (defun find-from-here ()
        "Find files from current buffer's directory"
        (interactive)
        (when buffer-file-name
          (consult-find (file-name-directory buffer-file-name))))

      (global-set-key (kbd "C-c d") 'find-from-here)
    '';

  gptelConfig =
    #lisp
    ''
      (use-package gptel
        :ensure t
        :config
        ;; Token access for GitHub Copilot
        (setq gptel-api-key
              (lambda ()
                (when-let ((auth (car (auth-source-search
                                     :host "github.copilot"
                                     :require '(:secret)))))
                  (let ((token (plist-get auth :secret)))
                    (if (functionp token)
                        (funcall token)
                      token)))))

        ;; Function to exchange GitHub token for Copilot token
        (defun gptel-copilot--exchange-token ()
          (let* ((github-token (gptel--get-api-key))
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

        (setq gptel-backend gptel-copilot-backend
              gptel-model 'claude-3.5-sonnet
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

    '';

  lspConfig =
    #lisp
    ''
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
        (setq lsp-nix-nixd-server-path "${pkgs.nixd}/bin/nixd")
        (setq lsp-enabled-clients '(nixd-lsp))
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
        :init (global-flycheck-mode))

    '';

  notesConfig =
    #lisp
    ''
      (message "Starting notes config...")

      ;; Basic settings
      (setq notes-directory "~/notes")
      (setq markdown-command "${pkgs.pandoc}/bin/pandoc")

      ;; Markdown configuration
      (use-package markdown-mode
        :ensure t
        :mode (("\\.md\\'" . markdown-mode)
               ("\\.markdown\\'" . markdown-mode)))

      ;; Basic org settings
      (use-package org
        :ensure t
        :bind
        (("C-c a" . org-agenda))
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

      ;; Add the global keybinding explicitly
      (global-set-key (kbd "C-c c") 'org-capture)

      (defun sanitize-filename (name)
      (downcase (replace-regexp-in-string "[^a-zA-Z0-9]" "-" name)))

(setq org-capture-templates
'(
("p" "Personal habit" entry
 (file (lambda () 
         (let ((name (read-string "File name: ")))
           (expand-file-name (concat (sanitize-filename name) ".org")
                           "~/notes/"))))
 "* TODO %^{Task description}
SCHEDULED: <%<%Y-%m-%d> +1d>
:PROPERTIES:
:CATEGORIES: personal habit
:CUSTOM_ID: %^{Custom id}
:END:")
("f" "Family habit" entry
 (file (lambda () 
         (let ((name (read-string "File name: ")))
           (expand-file-name (concat (sanitize-filename name) ".org")
                           "~/notes/"))))
 "* TODO %^{Task description}
SCHEDULED: <%<%Y-%m-%d> +1d>
:PROPERTIES:
:CATEGORIES: family habit
:CUSTOM_ID: %^{Custom id}
:END:")
("w" "Work habit" entry
 (file (lambda () 
         (let ((name (read-string "File name: ")))
           (expand-file-name (concat (sanitize-filename name) ".org")
                           "~/notes/"))))
 "* TODO %^{Task description}
SCHEDULED: <%<%Y-%m-%d> +1d>
:PROPERTIES:
:CATEGORIES: work habit
:CUSTOM_ID: %^{Custom id}
:END:")
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
           "* %^{Description}
SCHEDULED: %^T
:PROPERTIES:
:CUSTOM_ID: %^{ID}
:CATEGORIES: %^{Category|${categoriesString}}
:END:

  %?"
           :immediate-finish nil)
))

      ;; Face customization
      (with-eval-after-load 'org
        (set-face-attribute 'org-scheduled-previously nil
          :foreground "#d79921"
          :weight 'bold))


      ;; Date tracking functions
      (defun my/org-set-completed-date ()
        (when (equal "Done" (org-entry-get nil "STATUS"))
          (org-entry-put nil "COMPLETED"
            (format-time-string "[%Y-%m-%d %a]"))))

      (defun my/org-set-started-date ()
        (when (equal "In-Progress" (org-entry-get nil "STATUS"))
          (org-entry-put nil "STARTED"
            (format-time-string "[%Y-%m-%d %a]"))))

      (add-hook 'org-property-changed-functions
        (lambda (property value)
          (when (equal property "STATUS")
            (my/org-set-completed-date)
            (my/org-set-started-date))))

      ;; Conversion functions
      (defun convert-to-org ()
        "Convert current markdown buffer to org format."
        (interactive)
        (let* ((md-file (buffer-file-name))
               (org-file (concat (file-name-sans-extension md-file) ".org")))
          (when (and md-file (file-exists-p md-file))
            (call-process "${pkgs.pandoc}/bin/pandoc" nil nil nil
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
            (call-process "${pkgs.pandoc}/bin/pandoc" nil nil nil
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
    '';

  agendaConfig =
    #lisp
    ''
                  ;; Set this globally
                  (setq org-agenda-block-separator nil)
                  (setq org-agenda-window-setup 'only-window)
                  (setq org-agenda-timegrid-use-ampm t)
                  (setq org-agenda-time-leading-zero t)
                  (setq org-agenda-todo-keyword-format "%s")  
                  (setq org-agenda-include-diary t)  
                  (require 'diary-lib)

                  ;; Set up TODO keywords including SKIPPED state
                  (setq org-todo-keywords
                        '((sequence "TODO(t!)" "STARTED(s!)" "TODAY(O!)" "BLOCKED(b!)" "AWAITING-REVIEW(r!)" "AWAITING-QA(q!)""AWAITING-RELEASE(e)" "AWAITING-USER(u)" "REVIEWED(v)" "|" "DONE(d!)" "ABANDONED(a!)" "SKIPPED(k!)")))

                  ;; Set up faces for TODO states
                  (setq org-todo-keyword-faces
                        '(("TODO" . org-todo)
                          ("DONE" . org-done)
                          ("STARTED")
                          ("BLOCKED")
                          ("AWAITING-REVIEW")
                          ("AWAITING-QA")
                          ("AWAITING-RELEASE")
                          ("AWAITING-USER")
                          ("REVIEWED")
                          ("TODAY")
                          ("SKIPPED")))

                  ;; Function to handle skipping in agenda
                  (defun my/skip-task ()
                    "Mark current agenda task as SKIPPED and advance schedule"
                    (interactive)
                    (let* ((marker (or (org-get-at-bol 'org-marker)
                                      (org-agenda-error)))
                           (buffer (marker-buffer marker))
                           (pos (marker-position marker)))
                      (with-current-buffer buffer
                        (goto-char pos)
                        (org-todo "SKIPPED")
                        (org-schedule nil "+1d")))
                    )

                    (use-package org-super-agenda
                    :after org-agenda
                    :config
                    (setq org-super-agenda-header-map nil)  ; Disable super-agenda keybindings
                    (setq org-super-agenda-header-properties nil)
                    (org-super-agenda-mode)
                    )

                  (setq warning-suppress-types '((org-element)))

(setq org-agenda-custom-commands
      '(("d" "daily dashboard"
         ((agenda "Schedule"
                 ((org-agenda-span 'day)
                  (org-super-agenda-groups
                   '((:name "Today's Schedule"
                      :time-grid t
                      :property ("CATEGORIES" (lambda (value)
                        (and value
                        (string-match-p "event" value)))))
                      (:discard (:anything t)))
                  )))
          (alltodo "Today's Tasks"
                ((org-agenda-overriding-header "")
                  (org-super-agenda-groups
                  '((:name "Today's Tasks"
                  :todo "TODAY")
                  (:discard (:anything t))))))
          (agenda "Habits"
                 ((org-agenda-span 'day)
                  (org-agenda-sorting-strategy '((agenda todo-state-down alpha-up)))
                  (org-agenda-overriding-header "")  
                  (org-agenda-format-date "") 
                  (Org-agenda-remove-timeprops t)    
                  (org-super-agenda-groups
                   '(
                      (:name "One Offs"
                      :property ("CATEGORIES" (lambda (value)
                                             (message "Property value: '%s'" value)
                                             (and value
                                                  (string-match-p "one-off" value)))))
                     (:name "Personal Habits"
                      :property ("CATEGORIES" (lambda (value)
                                             (message "Property value: '%s'" value)
                                             (and value
                                                  (string-match-p "habit" value)
                                                  (string-match-p "personal" value)))))
                     (:name "Family Habits"
                      :property ("CATEGORIES" (lambda (value)
                                             (message "Property value: '%s'" value)
                                             (and value
                                                  (string-match-p "habit" value)
                                                  (string-match-p "family" value)))))
                     (:name "Work Habits"
                      :property ("CATEGORIES" (lambda (value)
                                             (message "Property value: '%s'" value)
                                             (and value
                                                  (string-match-p "habit" value)
                                                  (string-match-p "work" value)))))
                     (:discard (:anything t))
                   ))))
          ${mkTagAgendaBlocks tagAgendaBlocks}
         ))))

                  (defun refresh-org-agenda ()
                    "Refresh org agenda files and rebuild agenda view."
                    (interactive)
                    (setq org-agenda-files (list org-directory))
                    (when (get-buffer "*Org Agenda*")
                      (with-current-buffer "*Org Agenda*"
                        (org-agenda-redo t))))

                  (global-set-key (kbd "C-c r") 'refresh-org-agenda)

                    (setq org-agenda-time-grid-use-ampm t)
                    (setq org-agenda-with-times t)
                    (setq org-agenda-time-format "%I:%M%p")
                    (setq org-agenda-prefix-format
                        '((agenda . " ○ %t ")
                            (tags   . "○ ")
                            (todo   . "○ ")))

                    (use-package org-modern
                    :hook (org-agenda-finalize . org-modern-agenda)
                    :config
                    (global-org-modern-mode)
                    :custom
                    (org-modern-variable-pitch nil)
                    (org-modern-hide-stars t)        ; Hide leading stars
                    (org-modern-timestamp t)         ; Pretty timestamps
                    (org-modern-table t)            ; Pretty tables
                    (org-modern-label-offset 0.2) ; Reduces scaling to 20% of default
                    (org-modern-list '((43 . "➜")   ; List bullets (+ character)
                                        (45 . "–")))  ; List bullets (- character)
                    (org-modern-checkbox '((88 . "☑")   ; Checked box (X)
                                            (45 . "☐")    ; Empty box (-)
                                            (32 . "☐")))  ; Empty box (space)

                    (org-modern-tag t)              ; Pretty tags
                    (org-modern-priority t)         ; Pretty priorities

                    (org-modern-todo t)             ; Pretty todo keywords
                    (org-modern-block-fringe t)     ; Add fringe markers to blocks
                    (org-modern-block-name t)       ; Pretty source block names

                    ;;(org-modern-checkbox t)         ; Pretty checkboxes
                    ;;(org-modern-statistics t)       ; Pretty statistics cookies

                    (org-modern-table-vertical 1)   ; Table spacing
                    (org-modern-table-horizontal 0.2) ; Table spacing
                    )

                    (add-hook 'org-mode-hook (lambda ()
                          (variable-pitch-mode))
                    t)
                    ;;(debug-on-variable-change 'truncate-lines)

                    (custom-set-faces
                    '(org-document-info-keyword ((t (:height 1.0))))
                    '(org-document-title ((t (:height 140)))) ; Instead of default 2.0
                    '(org-level-1 ((t (:height 140))))  ; Instead of default 1.5
                    '(org-level-2 ((t (:height 140))))  ; Instead of default 1.3
                    '(org-level-3 ((t (:height 140)))) ; And so on
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

            (setq org-modern-todo-faces
                  '(("TODO" . (:weight bold :foreground "grey50"))
                    ("DONE" . (:weight bold :foreground "gray50"))
                    ("STARTED" . (:weight bold :foreground "grey60"))
                    ("BLOCKED" . (:weight bold :foreground "gray70"))
                    ("TODAY" . (:weight bold :foreground "red"))
                    ("AWAITING-REVIEW" . (:weight bold :foreground "orange"))
                    ("REVIEWED" . (:weight bold :foreground "gray80"))
                    ("AWAITING-QA" . (:weight bold :foreground "orange"))
                    ("AWAITING-RELEASE" . (:weight bold :foreground "orange"))
                    ("AWAITING-USER" . (:weight bold :foreground "orange"))
                    ("SKIPPED" . (:weight bold :foreground "gray50"))))

            (setq org-agenda-span 'day
                  org-agenda-start-on-weekday nil) ; Start on current day instead of Monday
    '';

  githubConfig =
    #lisp
    ''
          (require 'ghub)

          (defvar my/github-pr-file "~/notes/github-prs.org"
            "File to store GitHub PR todos.")

          (defvar my/github-pr-queries
            '(("Involved PRs" . "is:open is:pr involves:addisonbeck -author:addisonbeck")
              ("Renovate PRs" . "is:open is:pr involves:addisonbeck author:app/renovate")
              ))

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
                            (message "PR exists check returned: %S" exists-result)  ; New debug message
                            (unless exists-result
                              (message "PR doesn't exist, inserting")
                              (let ((insert-point (point)))
                                (message "Current point before insert: %S" insert-point)
                                (insert (format "* TODO %s
      :PROPERTIES:
      :PR_URL: %s
      :REPO: %s
      :AUTHOR: %s
      :TEAM: Platform
      :TYPE: Review
      :STATUS: Not-Started
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
    '';

  whichKeyConfig =
    #lisp
    ''
      (which-key-mode)
      (setq which-key-idle-delay 0.3
            which-key-prefix-prefix "→"
            which-key-sort-order 'which-key-key-order-alpha
            which-key-side-window-location 'bottom
            which-key-side-window-max-height 0.25)
    '';

  themeConfig =
    #lisp
    ''
      (use-package poet-theme
      :config
      (load-theme 'poet-dark t))
    '';

  packages = epkgs:
    with epkgs; [
      use-package
      evil
      evil-collection
      evil-org
      projectile
      company
      treemacs
      magit
      which-key
      consult
      gruvbox-theme
      solarized-theme
      poet-theme
      with-editor
      dashboard
      nerd-icons
      treesit-grammars.with-all-grammars
      nix-mode
      gptel
      markdown-mode
      cl-lib
      counsel
      vterm
      vertico
      consult
      marginalia
      orderless
      org
      markdown-mode
      lsp-mode
      lsp-ui
      company
      flycheck
      forge
      ghub
      org-modern
      org-super-agenda
    ];

  emacsConfig = pkgs.writeText "config.el" ''
    ;;; -*- lexical-binding: t -*-

    ${uiConfig}
    ${basicConfig}
    ${projectileConfig}
    ${serverConfig}
    ${treesitterConfig}
    ${gptelConfig}
    ${verticoConfig}
    ${notesConfig}
    ${agendaConfig}
    ${githubConfig}
    ${magitConfig}
    ${lspConfig}
    ${evilConfig}
    ${whichKeyConfig}
    ${fontConfig}
    ${themeConfig}
    (desktop-save-mode 1)
  '';
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-unstable.override {
        withTreeSitter = true;
      };
      config = emacsConfig;
      defaultInitFile = true;
      alwaysEnsure = true;
      extraEmacsPackages = packages;
    };
  };
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    pandoc
    nixd
    nodePackages.typescript-language-server
    omnisharp-roslyn
    marksman
    nodePackages.vscode-langservers-extracted
    nodePackages.eslint
    sqls
    rust-analyzer
    lua-language-server
    iosevka
    iosevka-bin
    (iosevka-bin.override {variant = "Aile";})
    (iosevka-bin.override {variant = "Etoile";})
  ];
  home.file.".emacs.d/diary".text = ''
    # This is my diary file
    # It can be empty but needs to exist
  '';
}

