{
  pkgs,
  lib,
  config,
  ...
}: let
  orgRoamFindNodePy = ../automated-emailing/org-roam-find-node-file.py;
  memory-css = ../automated-emailing/memory.css;
  memory-to-epub-file = pkgs.writeShellScriptBin "memory-to-epub-file" (builtins.readFile (pkgs.replaceVars ./memory-to-epub-file.sh {
    org-roam-find-node-file = "${orgRoamFindNodePy}";
    pandoc = "${pkgs.pandoc}/bin/pandoc";
    memory-css = "${memory-css}";
    python3 = "${pkgs.python3}/bin/python3";
    curl = "${pkgs.curl}/bin/curl";
    magick = "${pkgs.imagemagick}/bin/magick";
    nasa-token-path = lib.optionalString (config.sops.secrets ? nasa-token) config.sops.secrets.nasa-token.path;
  }));
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
      cp ${./code-reviews.org} code-reviews.org
      cp ${./emacs-everywhere.org} emacs-everywhere.org
      cp ${./text.org} text.org
      cp ${./bitwarden.org} bitwarden.org
      cp ${./binwarden.org} binwarden.org
      cp ${./org.org} org.org
      cp ${./files.org} files.org
      cp ${./sending-stuff-to-my-kindle.org} sending-stuff-to-my-kindle.org
      cp ${./org-present.org} org-present.org
      cp ${./olivetti.org} olivetti.org
      cp ${./autosave.org} autosave.org
      cp ${./spellcheck.org} spellcheck.org
      cp ${./jira.org} jira.org
      cp ${./lsp.org} lsp.org
      cp ${./addisonbeck-com.org} addisonbeck-com.org

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
    SOCKET="$HOME/.emacs.d/server/server"
    # First, ensure the daemon is running
    ${config.programs.emacs.finalPackage}/bin/emacsclient --socket-name="$SOCKET" -e "(+ 1 1)" >/dev/null 2>&1 || {
      echo "Starting Emacs daemon via launchctl..."
      /bin/launchctl kickstart -k gui/$(id -u)/org.gnu.emacs.daemon
      sleep 2
    }

    # Check if a visible frame exists (excluding terminal/daemon frames)
    FRAME_EXISTS=$(${config.programs.emacs.finalPackage}/bin/emacsclient --socket-name="$SOCKET" -e '(cl-some (lambda (f) (and (frame-visible-p f) (display-graphic-p f))) (frame-list))' 2>/dev/null)

    if [ "$FRAME_EXISTS" = "t" ]; then
      # Frame exists, just focus it
      ${config.programs.emacs.finalPackage}/bin/emacsclient --socket-name="$SOCKET" -n -e "(select-frame-set-input-focus (car (filtered-frame-list (lambda (f) (and (frame-visible-p f) (display-graphic-p f))))))" $@
    else
      # No frame exists, create one and focus it
      ${config.programs.emacs.finalPackage}/bin/emacsclient --socket-name="$SOCKET" -c -n -a "" -e '(progn (dashboard-refresh-buffer) (select-frame-set-input-focus (selected-frame)))' $@
    fi
  '';

  emacsclient-mobile-wrapper = pkgs.writeShellScriptBin "em" ''
    SOCKET="$HOME/.emacs.d/server/server"
    ${config.programs.emacs.finalPackage}/bin/emacsclient --socket-name="$SOCKET" -e "(+ 1 1)" >/dev/null 2>&1 || {
      echo "Starting Emacs daemon via launchctl..."
      /bin/launchctl kickstart -k gui/$(id -u)/org.gnu.emacs.daemon
      while ! ${config.programs.emacs.finalPackage}/bin/emacsclient --socket-name="$SOCKET" -e "(+ 1 1)" >/dev/null 2>&1; do
          sleep 0.5
      done
    }
    ${config.programs.emacs.finalPackage}/bin/emacsclient --socket-name="$SOCKET" -t -a "" -e "(dashboard-open)"
  '';
  puppeteer-cli-with-chrome = pkgs.puppeteer-cli.override {
    # chromium doesn't work on mac from nixpkgs
    chromium = pkgs.google-chrome;
  };

  #mcp-el-src = pkgs.fetchFromGitHub {
  #owner = "lizqwerscott";
  #repo = "mcp.el";
  #rev = "50f83fc4bac7cc01436bce5cd0f379aff435e083";
  #hash = "sha256-yWMjIao2ohzsprBkqbbAmTeKNnbFPbebUCKNfGnkxDc=";
  #};

  #emacsPackagesOverlay = self: super: {
  #mcp-el = super.trivialBuild {
  #pname = "mcp-el";
  #version = "git-${mcp-el-src.rev}";
  #src = mcp-el-src;
  #};
  #};

  # claude-code-ide.el - not on MELPA, must fetch from GitHub
  claude-code-ide-src = pkgs.fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364";
    sha256 = "sha256-tivRvgfI/8XBRImE3wuZ1UD0t2dNWYscv3Aa53BmHZE=";
  };

  claude-code-ide = pkgs.emacsPackages.trivialBuild {
    pname = "claude-code-ide";
    version = "unstable-2026-02-17";
    src = claude-code-ide-src;
    packageRequires = with pkgs.emacsPackages; [
      web-server
      vterm
      transient
    ];
  };
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-unstable.override {
        withTreeSitter = true;
        withImageMagick = true;
      };
      #override = emacsPackagesOverlay;
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
          magit-delta
          which-key
          consult
          gruvbox-theme
          with-editor
          dashboard
          nerd-icons
          treesit-grammars.with-all-grammars
          yaml-mode
          nix-mode
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
          consult-lsp
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
          mcp
          flyspell-correct
          hnreader
          rg
          mpv
          direnv
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
          org-roam-ql
          pkgs.aspell
          pkgs.aspellDicts.en
          emacs-everywhere
          #pr-review
          org-present
          org-inline-anim
          agent-shell
          web-server
          claude-code-ide
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
    yaml-language-server
    lua-language-server
    iosevka
    iosevka-bin
    (iosevka-bin.override {variant = "Aile";})
    (iosevka-bin.override {variant = "Etoile";})
    emacsclient-wrapper
    emacsclient-mobile-wrapper
    memory-to-epub-file
    mermaid-cli
    puppeteer-cli-with-chrome
    mpv-unwrapped
    yt-dlp
    ffmpeg
    ledger
    hledger
    jira-cli-go
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

  # Copy Emacs.app to a stable path so macOS TCC grants persist across rebuilds.
  # TCC tracks FDA by binary path; Nix store paths change every build, revoking access.
  # By keeping Emacs.app at ~/Applications/Emacs.app, the path is stable.
  home.activation.copyEmacsApp = lib.hm.dag.entryAfter ["writeBoundary"] ''
    app_src="${config.programs.emacs.finalPackage}/Applications/Emacs.app"
    app_dest="$HOME/Applications/Emacs.app"
    if [ -d "$app_src" ]; then
      $DRY_RUN_CMD mkdir -p "$HOME/Applications"
      if [ -d "$app_dest" ]; then
        $DRY_RUN_CMD chmod -R u+w "$app_dest"
        $DRY_RUN_CMD rm -rf "$app_dest"
      fi
      $DRY_RUN_CMD cp -r "$app_src" "$app_dest"
      $DRY_RUN_CMD chmod -R u+w "$app_dest"
    fi
  '';

  launchd.agents.emacs-daemon = {
    enable = true;
    config = {
      Label = "org.gnu.emacs.daemon";
      ProgramArguments = [
        "${config.home.homeDirectory}/Applications/Emacs.app/Contents/MacOS/Emacs"
        "--daemon=${config.home.homeDirectory}/.emacs.d/server/server"
      ];
      KeepAlive = false;
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
