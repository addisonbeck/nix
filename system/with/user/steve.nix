{
  inputs,
  pkgs,
  ...
}: let
  authorizedKeys = pkgs.writeText "authorized_keys" ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH me@bw
  '';
in {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  environment.shells = [pkgs.fish];
  programs.fish.enable = true;

  users.groups.steve = {};
  users.users.steve = {
    shell = pkgs.bash;
    isNormalUser = true;
    group = "steve";
    extraGroups = ["wheel" "minecraft"];
    openssh.authorizedKeys.keyFiles = ["${authorizedKeys}"];
  };
  home-manager.users.steve = {
    imports = [
      inputs.agenix.homeManagerModules.default
      ./with/trait/well-known-hosts.nix
      ./with/program/bash.nix
      ./with/program/git.nix
      ./with/program/direnv.nix
      ./with/program/tmux.nix
      ./with/program/d.nix
      inputs.nixvim.homeManagerModules.default
      ./with/program/neovim
      ./with/secret/github.nix
      ./with/program/lazygit.nix
      ./with/program/github-cli.nix
      ./with/program/direnv.nix
      ./with/program/ripgrep.nix
    ];

    home.sessionPath = [];
    home.sessionVariables = {EDITOR = "vim";};
    home.stateVersion = "24.05";
    home.enableNixpkgsReleaseCheck = false;
  };
}
