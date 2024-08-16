{ inputs, pkgs, lib, ... }: {
  programs.zsh.enable = true;
  programs.fish.enable = true;

  environment.shells = [ pkgs.zsh pkgs.fish ];

  users.knownUsers = [ "me" ];
  nix.trusted-users = [ "admin" "me" ];

  users.users.me = {
    shell = pkgs.fish;
  } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    uid = 503;
    name = "me";
    home = "/Users/me";
  } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
    users.users.me.isNormalUser = true;
    users.users.me.initialPassword = "me";
    users.users.me.extraGroups = [ "wheel" "docker" ];
  };

  home-manager.users.me = {
    imports = [
      inputs.stylix.homeManagerModules.stylix
      inputs.agenix.homeManagerModules.default
      inputs.nixvim.homeManagerModules.default
      ./with/trait/well-known-hosts.nix
      ./with/program/bash.nix
      ./with/program/stylix.nix
      ./with/program/git.nix
      ./with/program/direnv.nix
      ./with/program/tmux.nix
      ./with/program/kitty.nix
      ./with/secret/github.nix
      ./with/service/autoclone.nix
      { services.autoclone.enable = true; }
      ./with/development-environment/notes
      ./with/development-environment/bitwarden
      ./with/program/zsh.nix
      ./with/program/nixvim.nix
      ./with/program/lazygit.nix
      ./with/program/nixfmt.nix
      ./with/program/raycast.nix
      ./with/program/dotnet.nix
      ./with/program/nuke-docker.nix
      ./with/program/homebrew.nix
      ./with/development-environment/dotfiles
      ./with/development-environment/binwarden
      ./with/program/fzf.nix
      ./with/program/ripgrep.nix
      ./with/program/prettierd.nix
      ./with/program/starship.nix
      ./with/program/fish.nix
      ./with/development-environment/nix
      ./with/program/docker-desktop.nix
    ];

    home.stateVersion = "24.05";
    home.enableNixpkgsReleaseCheck = false;
  };
}
