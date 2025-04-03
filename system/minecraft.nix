{
  inputs,
  modulesPath,
  pkgs,
  ...
}: let
  modpack = pkgs.fetchPackwizModpack {
    url = "https://raw.githubusercontent.com/addisonbeck/nix/refs/heads/main/packwiz/bonesfamily/pack.toml";
    packHash = "sha256-zTZPvhsQgdgKv7kGsG3IrTVUcp27IccGe0FkpxvKPQE=";
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
          #jvmOpts = ''-Xms6144M -Xmx6144M -XX:+UseZGC'';
          #jvmOpts = ''-Xms2G -Xmx4G -XX:NewSize=1G -XX:MaxNewSize=2G -XX:SurvivorRatio=2 -XX:+DisableExplicitGC -d64 -XX:+UseConcMarkSweepGC -XX:+AggressiveOpts'';
          jvmOpts = ''-Xms4G -Xmx8G -XX:+UseG1GC -XX:MaxGCPauseMillis=50 -XX:+UseStringDeduplication -XX:+ParallelRefProcEnabled -XX:+UseNUMA -XX:-DisableExplicitGC'';
          enable = true;
          enableReload = false;
          package = inputs.nix-minecraft.legacyPackages.x86_64-linux.fabricServers.fabric-1_21_5;
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
