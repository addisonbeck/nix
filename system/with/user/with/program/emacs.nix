{ pkgs, ... }:

let
  dashboardConfig = ''
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
            dashboard-startup-banner 'logo
            dashboard-items '((recents . 5)
                            (projects . 5)
                            (bookmarks . 5))
            dashboard-banner-logo-title "Welcome to Emacs"
            dashboard-set-heading-icons t
            dashboard-set-file-icons t
            dashboard-show-shortcuts t  ;; Changed from dashboard-startupify-list
            dashboard-set-navigator t)
      ;; Set specific icons for dashboard
      (setq dashboard-heading-icons '((recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-bookmark")
                                    (projects  . "nf-oct-project"))))
  '';

  fontConfig = ''
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

  uiConfig = ''
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
  '';

  evilConfig = ''
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
  '';

  serverConfig = ''
    (require 'server)
    (defvar with-editor-emacsclient-executable "${pkgs.emacs}/bin/emacsclient")  ;; Declare variable before use
    (unless (server-running-p)
      (server-start))
  '';

  projectileConfig = ''
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
      (projectile-save-known-projects))
  '';

  treesitterConfig = ''
    ;; Tree-sitter configuration
    (require 'treesit)
    
    ;; Enable tree-sitter based modes by default
    (setq treesit-language-source-alist nil) ;; we're using nix-provided parsers
    
    ;; Configure nix-mode to use tree-sitter
    (require 'nix-mode)
    (defvar nix-mode-use-tree-sitter t)  ;; Declare variable before use
  '';

    ivyConfig = ''
    (use-package ivy
        :ensure t
        :config
        (ivy-mode 1)
        (setq ivy-use-virtual-buffers t)
        (setq ivy-count-format "(%d/%d) "))

    (use-package counsel
        :ensure t
        :config
        (counsel-mode 1))
    '';

  gptelConfig = ''
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

      ;; Set the backend configuration
      (setq gptel-backend gptel-copilot-backend
            gptel-model 'claude-3.5-sonnet 
            gptel-directory "~/chats"
            gptel-default-mode 'markdown-mode))
  '';

  themeConfig = ''
    (setq gruvbox-dark-mode t)
    (load-theme 'gruvbox t)
  '';

  packages = epkgs: with epkgs; [
    use-package
    evil
    evil-collection
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
    ivy
    counsel 
    vterm
  ];

  emacsConfig = pkgs.writeText "config.el" ''
    ;;; -*- lexical-binding: t -*-

    ${uiConfig}
    ${fontConfig}
    ${projectileConfig}
    ${dashboardConfig}
    ${serverConfig}
    ${evilConfig}
    ${treesitterConfig}
    ${gptelConfig}
    ${ivyConfig}
    ${themeConfig}
  '';

in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = (pkgs.emacs-unstable.override {
        withTreeSitter = true;
      });
      config = emacsConfig;
      defaultInitFile = true;
      alwaysEnsure = true;
      extraEmacsPackages = packages;
    };
  };
}
