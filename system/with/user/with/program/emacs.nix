{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs-unstable.override {
        withTreeSitter = true;
      };
      config = ./init.el;
      defaultInitFile = true;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs: with epkgs; [
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
    (iosevka-bin.override { variant = "Aile"; })
    (iosevka-bin.override { variant = "Etoile"; })
  ];

  home.file.".emacs.d/diary".text = ''
    # This is my diary file
    # It can be empty but needs to exist
  '';
}
