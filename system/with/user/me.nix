{
  inputs,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.agenix.nixosModules.default
    {
      age.secrets.me.file = ./with/secret/me.age;
      age.secrets.me.owner = "me";
      age.secretsDir = "/home/me/.secrets";
    }
  ];

  users.users.me = {
    isNormalUser = true;
    initialPassword = "me";
    extraGroups = ["wheel" "docker"];
  };
  
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.me = {
    imports = [ 
      ./with/trait/well-known-hosts.nix
      inputs.agenix.homeManagerModules.age
      {
        age.identityPaths = [ "/home/me/.secrets/me" ];
      }
      ./with/secret/github.nix
      ./with/program/bash.nix
      ./with/program/git.nix
      ./with/program/direnv.nix
      ./with/program/tmux.nix
      ./with/development-environment/nix
    ];

    home = {
      username = "me";
      homeDirectory = "/home/me";
    };

    home.stateVersion = "24.05";
    home.enableNixpkgsReleaseCheck = false;
  };
}
