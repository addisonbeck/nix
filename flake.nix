{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs-forked.url = "github:addisonbeck/nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    stylix.url = "github:danth/stylix";
    stylix.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:semnix/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    d.url = "github:addisonbeck/d/main";
    d.inputs.nixpkgs.follows = "nixpkgs";

    binwarden.url = "github:addisonbeck/binwarden/main";
    binwarden.inputs.nixpkgs.follows = "nixpkgs";

    nix-minecraft.url = "github:Infinidoge/nix-minecraft";
    nix-minecraft.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    agenix,
    nix-darwin,
    treefmt-nix,
    ...
  } @ inputs: let
    inherit (self) outputs;
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystemTypes = fn: nixpkgs.lib.genAttrs supportedSystems fn;
  in {
    nixosConfigurations = {
      minecraft = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs outputs nixpkgs;
          pkgs-forked = import inputs.nixpkgs-forked {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
        };
        modules = [./system/minecraft.nix];
      };
    };
    darwinConfigurations = {
      bw = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = {
          inherit inputs outputs nixpkgs;
          pkgs-forked = import inputs.nixpkgs-forked {
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
        };
        modules = [./system/bw.nix];
      };
      air = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = {
          inherit inputs outputs nixpkgs;
          pkgs-forked = import inputs.nixpkgs-forked {
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
        };
        modules = [./system/air.nix];
      };
    };
    devShells = forAllSystemTypes (system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      default = pkgs.mkShell {
        inputsFrom = with self.devShells.${system}; [
          building
          managing-secrets
          formatting
          editing
        ];
      };
      building = pkgs.mkShell {
        packages =
          [
            (pkgs.writeScriptBin "update" ''
              nix flake update
            '')
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            nix-darwin.packages.${system}.default
            (pkgs.writeScriptBin "rebuild" ''
              darwin-rebuild switch --flake .#"$1"
            '')
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            nix-darwin.packages.${system}.default
            (pkgs.writeScriptBin "rebuild" ''
              nixos-rebuild switch --flake .#"$1"
            '')
          ];
      };
      formatting = pkgs.mkShell {
        packages = [
          (treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix).config.build.wrapper
          (pkgs.writeScriptBin "check" ''
            if [ "$1" == "formatting" ]; then
              echo "Checking formatting..."
              treefmt --fail-on-change
            fi
          '')
          (pkgs.writeScriptBin "apply" ''
            if [ "$1" == "formatting" ]; then
              echo "Applying formatting..."
              treefmt
            fi
          '')
        ];
      };
      managing-secrets = pkgs.mkShell {
        packages = [
          agenix.packages.${system}.default
        ];
      };
      editing = pkgs.mkShell {
        packages = [
          pkgs.nixd
        ];
      };
    });
  };
}
