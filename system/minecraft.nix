{inputs, modulesPath, pkgs, ...}: {
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
	  extraRules = [{
	    commands = [
	      {
		command = "ALL";
		options = [ "NOPASSWD" ];
	      }
	    ];
	    groups = [ "wheel" ];
	  }];
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
	    package = inputs.nix-minecraft.legacyPackages.x86_64-linux.fabricServers.fabric-1_21_1;
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
                  url = "https://cdn.modrinth.com/data/P7dR8mSH/versions/iFnYBUfS/fabric-api-0.106.0%2B1.21.1.jar";
		  hash = "sha256-sUONHcxgqL+bZv17oWWaJCSiUZxwh/pYrQ6+l9edWKg=";
		};
		Geyser = pkgs.fetchurl {
		  url = "https://cdn.modrinth.com/data/wKkoqHrH/versions/LrOHtTFt/geyser-fabric-Geyser-Fabric-2.4.3-b689.jar";
         	  sha512 = "sha512-Sw4SX3nFYMblGSsgpVMOJxA7qiRfWPg6SU896Thmseb4hfg/jOkPkDd6jD/TvDfDiyCXIg9cPF725aFkfHvq/Q==";
		};
		Floodgate = pkgs.fetchurl {
                  url = "https://cdn.modrinth.com/data/bWrNNfkb/versions/wPa1pHZJ/Floodgate-Fabric-2.2.4-b36.jar";
                  hash = "sha256-ifzWrdZ4KJoQpFspdhmOQ+FJtwVMaGtfy4XQOcewV0Y=";
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
          allowedUDPPorts = [ 19132 ];
        };
      };
}
