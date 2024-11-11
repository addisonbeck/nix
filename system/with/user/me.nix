{
  inputs,
  pkgs,
  lib,
  ...
}: {
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  environment.shells = [pkgs.fish];
  programs.fish.enable = true;
  users.knownUsers = ["me"];

  users.users.me =
    {
      shell = pkgs.fish;
    }
    // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      uid = 502;
      name = "me";
      home = "/Users/me";
      createHome = true;
    }
    // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
      users.users.me.isNormalUser = true;
      users.users.me.initialPassword = "me";
      users.users.me.extraGroups = ["wheel" "docker"];
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
      #./with/service/autoclone.nix
      #{ services.autoclone.enable = true; }
      #./with/development-environment/notes
      #./with/development-environment/bitwarden
      ./with/program/zsh.nix
      ./with/program/nixvim.nix
      ./with/program/lazygit.nix
      #./with/program/nixfmt.nix
      #./with/program/raycast.nix
      ./with/program/dotnet.nix
      ./with/program/nuke-docker.nix
      ./with/program/homebrew.nix
      #./with/development-environment/dotfiles
      #./with/development-environment/binwarden
      ./with/program/fzf.nix
      ./with/program/ripgrep.nix
      ./with/program/prettierd.nix
      ./with/program/starship.nix
      ./with/program/fish.nix
      #./with/development-environment/nix
      #./with/program/docker-desktop.nix
      ./with/program/powershell.nix
      #./with/program/bitwarden-cli.nix
      ./with/program/mkcert.nix
      ./with/program/node.nix
      ./with/program/sed.nix
      ./with/program/github-cli.nix
      ./with/program/gh-dash.nix
      #./with/program/jira-cli.nix
      ./with/program/vscode.nix
      #./with/program/emacs.nix
      #./with/program/khal.nix
      #./with/secret/email.nix
      #./with/secret/bw-cal-client-id.nix
      #./with/secret/bw-cal-client-secret.nix
      #./with/program/python3.nix
      #./with/program/neomutt.nix
      #./with/program/weechat.nix
      #./with/secret/bw-mail-password.nix
      #./with/secret/gmail-password.nix
      #./with/secret/weechat-plugins-config.nix
      ./with/program/sqlite.nix
      ./with/program/d.nix
      ./with/program/binwarden.nix
      # SPLUNK APP
      ./with/program/python3.9.nix
      ./with/program/minecraft.nix
      # python v3.8.10
      # poetry
      # libmagic
      #./with/program/wezterm.nix
      ./with/program/newsboat.nix
      ./with/program/toggle-sleep-osx.nix
      ./with/program/ungoogled-chromium.nix
      ./with/program/mermaid-cli.nix
      ./with/program/markdown-mermaid-converter.nix
      ./with/program/dolphin-emu.nix
      ./with/program/xdelta.nix
      #./with/program/opentoonz.nix <- This doesn't work
      ./with/program/gimp.nix
      ./with/program/audacity.nix
    ];

    home.sessionPath = [];
    home.sessionVariables = {EDITOR = "vim";};
    home.stateVersion = "24.05";
    home.enableNixpkgsReleaseCheck = false;
  };
}
