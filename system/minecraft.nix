{
  inputs,
  modulesPath,
  pkgs,
  ...
}: let
  modpack = pkgs.fetchPackwizModpack {
    url = "https://raw.githubusercontent.com/addisonbeck/nix/refs/heads/main/packwiz/bonesfamily/pack.toml";
    packHash = "sha256-DcJeAxOGBA6pksvrwEsIT+MBrYYIZYPZd5iBcY4XTgM=";
  };
in {
  imports = [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
    inputs.home-manager.nixosModules.home-manager
    ./with/nix.nix
    ./with/home-manager.nix
    ./with/trait/allow-unfree-packages.nix
    ./with/trait/ssh-enabled.nix
    ./with/trait/has-swapfile.nix
    {has-swapfile.sizeGb = 4;}
    ./with/user/root.nix
    ./with/user/steve.nix
    {
      security.sudo = {
        enable = true;
        extraRules = [
          {
            commands = [
              {
                command = "ALL";
                options = ["NOPASSWD"];
              }
            ];
            groups = ["wheel"];
          }
        ];
      };
    }
    inputs.nix-minecraft.nixosModules.minecraft-servers
    {
      services.minecraft-servers = {
        enable = true;
        eula = true;
        managementSystem.systemd-socket = {
          enable = true;
        };
        # dataDir = "/home/steve/servers/";
        # user = "steve";
        # group = "steve";
        servers.bonesfamily = {
          #jvmOpts = ''-Xmx6G -Xms6G -XX:+UseG1GC -Dsun.rmi.dgc.server.gcInterval=2147483646 -XX:+UnlockExperimentalVMOptions -XX:G1NewSizePercent=20 -XX:G1ReservePercent=20 -XX:MaxGCPauseMillis=50 -XX:G1HeapRegionSize=32M'';
          jvmOpts = ''-Xms6144M -Xmx6144M -XX:+UseZGC'';
          enable = true;
          enableReload = false;
          package = inputs.nix-minecraft.legacyPackages.x86_64-linux.fabricServers.fabric-1_21_4;
          openFirewall = true;
          autoStart = true;
          serverProperties = {
            default-player-permission-level = "visitor";
            motd = "Bones bones bones";
            difficulty = "easy";
            gamemode = "survival";
          };
          files = {
            "config/Geyser-Fabric/config.yml".value = {
              remote."auth-type" = "floodgate";
            };
          };
          symlinks = {
            "mods" = "${modpack}/mods";
          };
        };
      };
    }
  ];
  nixpkgs.overlays = [inputs.nix-minecraft.overlay];

  networking = {
    firewall = {
      enable = true;
      allowedUDPPorts = [19132];
    };
  };
}
