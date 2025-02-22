{
  pkgs,
  config,
  ...
}: let
  tangledInit =
    pkgs.runCommand "init.el" {
      nativeBuildInputs = [(pkgs.emacs.pkgs.withPackages (epkgs: [epkgs.org]))];
      src = ./init.org;
    } ''
      set -x  # Enable debug output

      echo "Copying org file..."
      cp $src init.org

      echo "Creating tangle script..."
      cat > tangle.el <<EOF
      (require 'org)
      (setq org-confirm-babel-evaluate nil)
      (setq debug-on-error t)
      (condition-case err
          (org-babel-tangle-file "init.org" "init.el" "emacs-lisp")
        (error
         (message "Error during tangling: %S" err)
         (kill-emacs 1)))
      EOF

      echo "Running Emacs to tangle..."
      emacs --batch -Q -l tangle.el || {
        echo "Tangling failed!"
        exit 1
      }

      echo "Moving tangled file to output..."
      mv init.el $out || {
        echo "Failed to move output file!"
        exit 1
      }
    '';
  emacsclient-wrapper = pkgs.writeShellScriptBin "ec" ''
    # First, ensure the daemon is running
    ${config.programs.emacs.package}/bin/emacsclient -e "(+ 1 1)" >/dev/null 2>&1 || {
      echo "Starting Emacs daemon..."
      ${config.programs.emacs.package}/bin/emacs --daemon
    }

    # Check if a visible frame exists (excluding terminal/daemon frames)
    FRAME_EXISTS=$(${config.programs.emacs.package}/bin/emacsclient -e '(cl-some (lambda (f) (and (frame-visible-p f) (display-graphic-p f))) (frame-list))' 2>/dev/null)

    if [ "$FRAME_EXISTS" = "t" ]; then
      # Frame exists, just focus it
      ${config.programs.emacs.package}/bin/emacsclient -n -e "(select-frame-set-input-focus (car (filtered-frame-list (lambda (f) (and (frame-visible-p f) (display-graphic-p f))))))"
    else
      # No frame exists, create one and focus it
      ${config.programs.emacs.package}/bin/emacsclient -c -n -a "" -e '(progn (dashboard-refresh-buffer) (select-frame-set-input-focus (selected-frame)))'
    fi
  '';
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-unstable.override {
        withTreeSitter = true;
      };
      config = tangledInit;
      defaultInitFile = true;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs:
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
          org-super-agenda
          elfeed
          elfeed-protocol
          exec-path-from-shell
        ];
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
    emacsclient-wrapper
  ];

  home.file.".emacs.d/diary".text = ''
    # This is my diary file
    # It can be empty but needs to exist
  '';

  # Launchd service for Emacs daemon
  launchd.agents.emacs-daemon = {
    enable = true;
    config = {
      Label = "org.gnu.emacs.daemon";
      ProgramArguments = [
        "${config.programs.emacs.package}/bin/emacs"
        "--daemon"
      ];
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/.emacs.d/daemon.log";
      StandardErrorPath = "${config.home.homeDirectory}/.emacs.d/daemon.error.log";
    };
  };
}
