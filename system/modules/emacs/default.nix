{
  pkgs,
  config,
  ...
}: let
  tangledInit =
    pkgs.runCommand "init.el" {
      nativeBuildInputs = [(pkgs.emacs.pkgs.withPackages (epkgs: [epkgs.org]))];
      src = ./.;
    } ''
      set -x  # Enable debug output

      echo "Copying org files..."
      cp ${./auth.org} auth.org
      cp ${./projects.org} projects.org
      cp ${./package-sources.org} package-sources.org
      cp ${./mode-line.org} mode-line.org
      cp ${./ui.org} ui.org
      cp ${./core.org} core.org
      cp ${./frame-management.org} frame-management.org
      cp ${./theme.org} theme.org
      cp ${./gptel.org} gptel.org
      cp ${./memory-tools.org} memory-tools.org
      cp ${./code-reviews.org} code-reviews.org
      cp ${./emacs-everywhere.org} emacs-everywhere.org
      cp ${./text.org} text.org
      cp ${./bitwarden.org} bitwarden.org
      cp ${./binwarden.org} binwarden.org
      cp ${./org.org} org.org
      cp ${./files.org} files.org
      cp ${./gptel-file-tools.org} gptel-file-tools.org
      cp ${./gptel-execute-elisp-tools.org} gptel-execute-elisp-tools.org
      cp ${./gptel-shell-command-tools.org} gptel-shell-command-tools.org

      echo "Copying tangle script..."
      cp ${./tangle-script.el} tangle.el

      echo "Running Emacs to tangle..."
      emacs --batch -Q -l tangle.el

      echo "Checking if init.el was created..."
      if [ ! -f "init.el" ]; then
        echo "ERROR: init.el was not created!"
        echo "Files in current directory:"
        ls -la
        exit 1
      fi

      echo "Content of generated init.el:"
      cat init.el
      echo "END OF INIT.EL"

      echo "Moving tangled file to output..."
      cp init.el $out
    '';

  # Improved emacsclient wrapper that properly handles frame reuse
  emacsclient-wrapper = pkgs.writeShellScriptBin "ec" ''
    # First, ensure the daemon is running
    ${config.programs.emacs.finalPackage}/bin/emacsclient -e "(+ 1 1)" >/dev/null 2>&1 || {
      echo "Starting Emacs daemon..."
      ${config.programs.emacs.finalPackage}/bin/emacs --daemon
    }

    # Check if a visible frame exists (excluding terminal/daemon frames)
    FRAME_EXISTS=$(${config.programs.emacs.finalPackage}/bin/emacsclient -e '(cl-some (lambda (f) (and (frame-visible-p f) (display-graphic-p f))) (frame-list))' 2>/dev/null)

    if [ "$FRAME_EXISTS" = "t" ]; then
      # Frame exists, just focus it
      ${config.programs.emacs.finalPackage}/bin/emacsclient -n -e "(select-frame-set-input-focus (car (filtered-frame-list (lambda (f) (and (frame-visible-p f) (display-graphic-p f))))))" $@
    else
      # No frame exists, create one and focus it
      ${config.programs.emacs.finalPackage}/bin/emacsclient -c -n -a "" -e '(progn (dashboard-refresh-buffer) (select-frame-set-input-focus (selected-frame)))' $@
    fi
  '';
    puppeteer-cli-with-chrome = pkgs.puppeteer-cli.override {
    # chromium doesn't work on mac from nixpkgs
    chromium = pkgs.google-chrome;
    };

  mcp-el-src = pkgs.fetchFromGitHub {
    owner = "lizqwerscott";
    repo = "mcp.el";
    rev = "50f83fc4bac7cc01436bce5cd0f379aff435e083";
    hash = "sha256-yWMjIao2ohzsprBkqbbAmTeKNnbFPbebUCKNfGnkxDc=";
  };

  emacsPackagesOverlay = self: super: {
    mcp-el = super.trivialBuild {
      pname = "mcp-el";
      version = "git-${mcp-el-src.rev}";
      src = mcp-el-src;
    };
  };

in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-unstable.override {
        withTreeSitter = true;
      };
      override = emacsPackagesOverlay;
      config = tangledInit;
      defaultInitFile = true;
      alwaysEnsure = true;
      /*
      Here's how to override one of these, if I ever need that again.

         (gptel.overrideAttrs (old: {
           src = pkgs.fetchFromGitHub {
             owner = "karthink";
             repo = "gptel";
             rev = "229f7c689c67f993c0bb68052ef0f365b165dcd3";
             sha256 = pkgs.lib.fakeSha256;
           };
         }))
      */
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
          ox-jira
          all-the-icons
          # ob-async
          ob-mermaid
          google-maps
          calfw
          calfw-org
          ox-json
          org-make-toc
          avy
          rg
          mcp-el
          flyspell-correct
          hnreader
          rg
          mpv
          direnv
          aidermacs
          olivetti
          org-modern
          breadcrumb
          oauth2
          smtpmail-multi
          rustic
          ledger-mode
          org-ql
          miniedit
          mini-frame
          org-roam
          org-roam-ui
          pkgs.aspell
          pkgs.aspellDicts.en
          emacs-everywhere
          #pr-review
        ];
    };
  };

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    pandoc
    nixd
    nodePackages.typescript
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
    mermaid-cli
    puppeteer-cli-with-chrome
    mpv-unwrapped
    yt-dlp
    ffmpeg
    ledger
    hledger
(writeShellScriptBin "emacseverywhere" ''
set -euo pipefail

${config.programs.emacs.finalPackage}/bin/emacsclient -e "(emacs-everywhere)"
'')
  ];

  home.sessionVariables.ASPELL_DICT_DIR = "${pkgs.aspellDicts.en}/lib/aspell";

  home.file.".emacs.d/diary".text = ''
    # This is my diary file
    # It can be empty but needs to exist
  '';

  # Add symlink to debug the generated config
  home.file.".emacs.d/generated-init.el" = {
    source = tangledInit;
  };

  launchd.agents.emacs-daemon = {
    enable = true;
    config = {
      Label = "org.gnu.emacs.daemon";
      ProgramArguments = [
        "${config.programs.emacs.finalPackage}/bin/emacs"
        "--daemon"
      ];
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/.emacs.d/daemon.log";
      StandardErrorPath = "${config.home.homeDirectory}/.emacs.d/daemon.error.log";
      NetworkState = true;
      LimitLoadToSessionType = "Aqua";
    };
  };

  #services.emacs = {
    #enable = true;
    # maybe i just don't need to specifiy package
    #package = config.programs.emacs.package;
    #client.enable = true;

    #socketActivation.enable = true;  # More reliable daemon management
  #};
  
}
