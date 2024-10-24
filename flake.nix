# dotnet test -v quiet --nologo -l:"console;verbosity=error" | grep -Ei "^(Passed|Failed)!"
# dotnet test test/Core.Test/Core.Test.csproj -v quiet --nologo
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

    treefmt-nix = {
      url = "github:semnix/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    d = {
      url = "github:addisonbeck/d/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    binwarden = {
      url = "github:addisonbeck/binwarden/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-minecraft = {
      url = "github:Infinidoge/nix-minecraft";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixvim,
    agenix,
    nix-darwin,
    d,
    binwarden,
    treefmt-nix,
    ...
  } @ inputs: let
    inherit (self) outputs;
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystemTypes = fn: nixpkgs.lib.genAttrs supportedSystems fn;
  in {
    nixosConfigurations = {
      # vm = nixpkgs.lib.nixosSystem {
      #   system = "aarch64-linux";
      #   specialArgs = { inherit inputs outputs nixpkgs; };
      #   modules = [ ./system/vm.nix ];
      # };
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
      # nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake github:addisonbeck/nix#bw
      # nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake .#bw
      # darwin-rebuild switch --flake .#bw
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
        inputsFrom = with self.devShells.${system}; [formatting];
        packages = [
          agenix.packages.${system}.default
          #nix-darwin.packages.${system}.default
        ];
      };
      formatting = pkgs.mkShell {
        packages = [
          (treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix).config.build.wrapper
          (pkgs.writeScriptBin "check-formatting" ''
            treefmt --fail-on-change
          '')
          (pkgs.writeScriptBin "apply-formatting" ''
            treefmt
          '')
        ];
      };
    });
  };
}
