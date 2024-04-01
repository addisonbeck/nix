{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  nixpkgs = {
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "me";
    homeDirectory = "/home/me";
  };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  # home.packages = with pkgs; [ steam ];

  # Enable home-manager and git
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "addisonbeck";
    userEmail = "github@addisonbeck.com";
    aliases = {
      a = "add";
      p = "push";
      pl = "pull";
      f = "fetch --all";
      r = "restore";
      l = "log";
      li = "list";
      d = "diff";
      rs = "restore";
      g = "grep";
      c = "checkout";
      s = "status";
      bigreset = "reset (git merge-base master (git rev-parse --abbrev-ref
      HEAD))";
      clog = "reflog --grep-reflog='commit' --format=\"format:'%C(yellow bold
      dim)[%h] %C(nodim)%gd %C(white)%an %C(italic nobold)%s %C(magenta
      noitalic)%d'\"";
      ir = "rebase -i origin/HEAD";
      sh = "!f() { rev=\${1-HEAD}; git difftool $rev^ $rev; }; f";
      purge = "!git branch | grep -v \" master$\" | xargs git branch -D";
    };
    signing = {
      key = "ssh-ed25519
      AAAAC3NzaC1lZDI1NTE5AAAAILZ93u2ED0EnjiGc+gcbCl9pC+uPhArzu/Y2pURZ+D91
      git@addisonbeck.com";
      signByDefault = true;
    };
    ignores = [
      "*.swp"
    ];
    delta = {
      enable = true;
      options = {
        side-by-side = false;
        line-numbers = true;
      };
    };
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
