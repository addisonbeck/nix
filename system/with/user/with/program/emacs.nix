{pkgs, lib, ...}: let
  habitPrompts = {
    "lincolns-journal-prompt" = "What did he say?";
    "cook-dinner" = "What did you make?";
    "read-something" = "What did you read?";
    "write-something" = "What did you write?";
    "take-a-picture" = "Caption it";
    "encourage-the-kids-to-take-a-shower" = "Did they?";
    "scare-at-least-one-kid" = "What happened?";
    "praise-at-least-one-kid" = "What happened?";
    "practice-spirituality" = "What did you do?";
    "make-something" = "What did you make?";
    "fix-something" = "What did you fix?";
    "clean-something" = "What did you clean?";
    "plan-something" = "What did you plan?";
    "play-something" = "What did you play?";
    "do-some-beeep-work" = "What beeep stuff did you work on?";
    "participate-in-the-forums" = "What happened?";
    "participate-on-github" = "What happened?";
    "meal-plan" = "What did you do?";
    "text-somebody" = "Who?";
    "advocate-for-the-open-source-activism-working-group" = "How?";
    "do-some-sprint-work" = "What?";
    "force-yourself-to-network" = "Who?";
  };

  habitPromptsElisp = 
    let
      makePromptEntry = id: prompt: ''(cons "${id}" "${prompt}")'';  # Changed this line
      promptList = lib.mapAttrsToList makePromptEntry habitPrompts;
    in
    "(list ${builtins.concatStringsSep " " promptList})";

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
      (org-agenda-prefix-format
       '((tags . "%-l%-16(org-entry-get nil \"STATUS\")")))
      (org-agenda-sorting-strategy
       '((tags 'property-up "STATUS")))
      (org-agenda-keep-with-parent t)))
  '';

  mkTagAgendaBlocks = blocks:
    builtins.concatStringsSep "\n" (map mkTagAgendaBlock blocks);

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
      ;; Set default font
      (set-face-attribute 'default nil
                          :family "MonaspiceAr Nerd Font Mono"
                          :height 140)  ; Adjust size to your preference

      ;; Set variable-pitch font (for mixed-pitch-mode if you use it)
      (set-face-attribute 'variable-pitch nil
                          :family "MonaspiceAr Nerd Font Mono"
                          :height 140)

      ;; Set the font for specific Unicode ranges (symbols, etc)
      (set-fontset-font t 'symbol "MonaspiceAr Nerd Font Mono" nil)
    '';

  uiConfig =
    #lisp
    ''
      (setq default-frame-alist
            '((background-color . "#282828")
              (foreground-color . "#ebdbb2")
              (menu-bar-lines . 0)
              (tool-bar-lines . 0)
              (vertical-scroll-bars)
              (left-fringe . 0)
              (right-fringe . 0)
              (internal-border-width . 0)
              (fullscreen . maximized)))
      (setq inhibit-startup-message t)
      (setq initial-frame-alist default-frame-alist)
      (setq-default mode-line-format nil)
      (advice-add #'display-startup-echo-area-message :override #'ignore)
      (desktop-save-mode 1)  ;; Added this line
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

      (use-package evil-org
        :ensure t
        :after org
        :hook (org-mode . evil-org-mode)
        :config
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys)

    (with-eval-after-load 'evil
    (evil-define-key 'normal org-mode-map
        "gx" 'org-open-at-point))

      ;; Add this after-load hook
      (with-eval-after-load 'org-agenda
        (evil-define-key 'motion org-agenda-mode-map "g" nil)  ; Clear existing g binding
        (evil-define-key 'motion org-agenda-mode-map (kbd "gx") 'org-agenda-open-link))
      (setq org-link-frame-setup
        '((file . find-file))))
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
      
      (message "Basic settings done")

      ;; Markdown configuration
      (use-package markdown-mode
        :ensure t
        :mode (("\\.md\\'" . markdown-mode)
               ("\\.markdown\\'" . markdown-mode)))
      (message "Markdown config done")

      ;; Basic org settings
      (use-package org
        :ensure t
        :init
        (message "Starting org config...")
        :bind
        (("C-c a" . org-agenda))
        :config
        (setq org-directory "~/notes")
        (setq org-agenda-files (list org-directory))
        (setq org-global-properties
              '(("STATUS_ALL" . "Not-Started\\|In-Progress\\|Blocked\\|Done")
                ("TYPE_ALL" . "Bug\\|Feature\\|Chore\\|Spike\\|Review")))
        (setq org-clock-persist 'history
              org-clock-idle-time 15
              org-clock-into-drawer t
              org-log-into-drawer t)
        (org-clock-persistence-insinuate)
        (message "Org config done"))

      (message "Starting face customization...")
      (with-eval-after-load 'org
        (set-face-attribute 'org-scheduled-previously nil
          :foreground "#d79921"
          :weight 'bold))
      (message "Face customization done")

      ;; Date tracking functions
      (message "Setting up date tracking...")
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
      (message "Date tracking done")

      ;; Conversion functions
      (message "Setting up conversion functions...")
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
                ;; Create new file if it doesn't exist
                (with-temp-file new-file
                (insert "#+TITLE: " custom-id "\n\n")
                (insert region-content))
                ;; Delete original content
                (delete-region (region-beginning) (region-end))
                ;; Insert link to new file
                (insert (format "[[file:%s][%s]]\n" new-file custom-id))
                (message "Moved to %s" new-file))
            (message "No CUSTOM_ID property found!")))))
      (message "Conversion functions done")
      (message "Notes config complete")
    '';

  agendaConfig =
    #lisp
    ''
      ;; Set this globally
      (setq org-agenda-block-separator nil)
      (setq org-agenda-window-setup 'only-window)

      (setq org-agenda-custom-commands
        '(("d" "daily dashboard"
           ((agenda ""
             ((org-agenda-span 'day)
              (org-agenda-format-date "%A, %B %d %Y")
              (org-agenda-prefix-format '((agenda . "%-12t")))
              (org-agenda-entry-types '(:scheduled))
              (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'done))
              (org-agenda-sorting-strategy
               '((agenda time-up priority-down category-keep)))))

            (agenda ""
             ((org-agenda-span 28)
              (org-agenda-start-day "+1d")
              (org-agenda-start-on-weekday nil)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if
                  'notscheduled
                  'scheduled-past
                  'scheduled-future 28
                  'regexp "\\+[0-9]+[d]>"
                  'todo 'done))
              (org-agenda-prefix-format '((agenda . "%-12t")))
              (org-agenda-show-all-dates nil)
              (org-agenda-sorting-strategy
               '((agenda time-up priority-down category-keep)))
              (org-agenda-overriding-header
               (propertize "\n28 Day Forecast\n" 'face '(:weight bold)))))

            ${mkTagAgendaBlocks tagAgendaBlocks}))))

      (defun refresh-org-agenda ()
        "Refresh org agenda files and rebuild agenda view."
        (interactive)
        (setq org-agenda-files (list org-directory))
        (when (get-buffer "*Org Agenda*")
          (with-current-buffer "*Org Agenda*"
            (org-agenda-redo t))))

      (global-set-key (kbd "C-c r") 'refresh-org-agenda)
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
    '';

habitConfig = 
  #lisp
  ''
    (defvar my/habit-prompts ${habitPromptsElisp}
     "Alist of CUSTOM_ID to prompt question for habits that require logging.")

    (defun my/log-habit-note ()
    "Add notes to habit if it has a configured prompt"
    (when (string-equal-ignore-case (org-get-todo-state) "DONE") 
        (let* ((custom-id (org-entry-get nil "CUSTOM_ID"))
            (prompt-pair (assoc custom-id my/habit-prompts)))
        (when prompt-pair
            (let* ((prompt (cdr prompt-pair))
                (note (read-string (concat prompt " ")))
                (timestamp (format-time-string "[%Y-%m-%d %a]"))
                (entry (format "%s: %s" timestamp note)))
            (org-add-note)  ; Changed from org-add-log-setup and org-store-log-note
            (insert entry))))))


    (add-hook 'org-after-todo-state-change-hook #'my/log-habit-note)
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
      (setq gruvbox-dark-mode t)
      (load-theme 'gruvbox t)
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
      org-bullets
      markdown-mode
      lsp-mode
      lsp-ui
      company
      flycheck
      forge
      ghub
    ];

  emacsConfig = pkgs.writeText "config.el" ''
    ;;; -*- lexical-binding: t -*-

    ${uiConfig}
    ${basicConfig}
    ${fontConfig}
    ${projectileConfig}
    ${serverConfig}
    ${treesitterConfig}
    ${gptelConfig}
    ${verticoConfig}
    ${notesConfig}
    ${agendaConfig}
    ${githubConfig}
    ${evilConfig}
    ${magitConfig}
    ${habitConfig}
    ${lspConfig}
    ${whichKeyConfig}
    ${themeConfig}
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
  ];
}
