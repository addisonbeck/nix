{
  inputs,
  lib,
  config,
  pkgs,
  modulesPath,
  agenix,
  ...
}: {
  imports = [ (modulesPath + "/virtualisation/digital-ocean-config.nix") ];


  swapDevices = [{ device = "/swapfile"; size = 1024 * 4; }];
  networking.firewall.allowedTCPPorts = [ 80 443 22 ];

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
    };
  };

  # This will add each flake input as a registry
  # To make nix3 commands consistent with your flake
  nix.registry = (lib.mapAttrs (_: flake: {inherit flake;})) ((lib.filterAttrs (_: lib.isType "flake")) inputs);

  # This will additionally add your inputs to the system's legacy channels
  # Making legacy nix commands consistent as well, awesome!
  nix.nixPath = ["/etc/nix/path"];
  environment.etc =
    lib.mapAttrs'
    (name: value: {
      name = "nix/path/${name}";
      value.source = value.flake;
    })
    config.nix.registry;

  nix.settings = {
    experimental-features = "nix-command flakes";
    auto-optimise-store = true;
  };

  networking.hostName = "dev";

  users.users = {
    me = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIITJNv1ewE1vKWYdXkpCmuQqvc0js217YB36FZq9qPMs nixos-dev-deployment-key"
      ];
      extraGroups = ["wheel" "docker"];
    };
  };

  environment.systemPackages = with pkgs; [
    git
    vim
    neovim
    curl
    tmux
    home-manager
    bitwarden-cli
    agenix.packages.x86_64-linux.default
  ];

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  security.sudo.extraRules = [
    {  
      users = [ "me" ];
      commands = [
        { 
          command = "ALL" ;
          options= [ "NOPASSWD" ];
        }
      ];
    }
  ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
