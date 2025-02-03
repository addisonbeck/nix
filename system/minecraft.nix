{
  inputs,
  modulesPath,
  pkgs,
  ...
}: {
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
        #        managementSystem.tmux = {
        #   enable = true;
        #   socketPath = name: "/home/steve/sessions/${name}.sock";
        # };
        # dataDir = "/home/steve/servers/";
        # user = "steve";
        # group = "steve";
        servers.bonesfamily = {
          jvmOpts = ''-Xmx6G -Xms6G -XX:+UseG1GC -Dsun.rmi.dgc.server.gcInterval=2147483646 -XX:+UnlockExperimentalVMOptions -XX:G1NewSizePercent=20 -XX:G1ReservePercent=20 -XX:MaxGCPauseMillis=50 -XX:G1HeapRegionSize=32M'';
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
            mods = pkgs.linkFarmFromDrvs "mods" (builtins.attrValues {
              FabricAPI = pkgs.fetchurl {
                url = "https://cdn.modrinth.com/data/P7dR8mSH/versions/15ijyoD6/fabric-api-0.113.0%2B1.21.4.jar";
                hash = "sha256-V6sJzn/0qgbpZIjbjpbQynvHqjcRcNkVqaKmmamXRkU=";
              };
              Geyser = pkgs.fetchurl {
                url = "https://cdn.modrinth.com/data/wKkoqHrH/versions/aq2OFs4I/geyser-fabric-Geyser-Fabric-2.6.0-b754.jar";
                sha256 = "sha256-cc8i208l3wcpwmMoHgOAMx3kdH3apW6m3E9ZARToxdk=";
              };
              Floodgate = pkgs.fetchurl {
                url = "https://cdn.modrinth.com/data/bWrNNfkb/versions/nyg969vQ/Floodgate-Fabric-2.2.4-b43.jar";
                hash = "sha256-UbF/VyMY4Eo0SbtZqsCr0739kYbYK2ku0URZNjUPrSU=";
              };
            });
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
