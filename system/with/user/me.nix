{ inputs, pkgs, lib, ... }: {
  users.users.me = {
    shell = pkgs.zsh;
  } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
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
    ];

    home.sessionPath = [
      "/Users/me/bin/binwarden"
      "/Users/me/bin"
      "/opt/homebrew/bin"
      "/opt/homebrew/sbin"
    ];
    home.stateVersion = "24.05";
    home.enableNixpkgsReleaseCheck = false;
    launchd.agents.raycast = {
      enable = true;
      config = {
        ProgramArguments =
          [ "${pkgs.raycast}/Applications/Raycast.app/Contents/MacOS/Raycast" ];
        KeepAlive = true;
        RunAtLoad = true;
      };
    };
  };
}
