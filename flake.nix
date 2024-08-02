{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixvim,
    agenix,
    ...
  } @ inputs: let
    inherit (self) outputs;
    supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
      pkgs = import nixpkgs { inherit system; };
      nixvim = nixvim.legacyPackages."${system}";
    });
  in {
    nixosConfigurations = {
      vm = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit inputs outputs nixpkgs; };
        modules = [ ./system/vm.nix ];
      };
    };
    devShells = forEachSupportedSystem ({ pkgs, nixvim }: {
      default = pkgs.mkShell {
        packages = [
          (nixvim.makeNixvim {
            colorschemes.gruvbox.enable = true;
	    plugins.telescope.enable = true;
	    plugins.telescope.extensions.file-browser.enable = true;
	    plugins.telescope.extensions.file-browser.settings.hidden.file_browser = true;
	    plugins.telescope.extensions.file-browser.settings.hidden.folder_browser = true;
          })
          pkgs.lazygit 
        ];
      };
    });
  };
}
