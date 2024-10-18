{inputs, modulesPath, ...}: {
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
	services.minecraft-server = {
	  enable = true;
	  eula = true;
	};

	servers.survival = {
	  enable = true;
	  enableReload = true;
	    package = inputs.nix-minecraft.legacyPackages.x86_64-linux.paperServers.paper-1_20_4;
	};
    }
  ];
}
