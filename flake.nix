{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # This isn't actually used anymore, but leaving it setup in case I need
    # to revive my fork some other time.
    #nixpkgs-forked.url = "github:addisonbeck/nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Deprecated, moved to sops.
    # Leaving for a bit in case I messed something up
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:semnix/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    #d.url = "github:addisonbeck/d/main";
    #d.inputs.nixpkgs.follows = "nixpkgs";

    binwarden.url = "github:addisonbeck/binwarden/main";
    binwarden.inputs.nixpkgs.follows = "nixpkgs";

    nix-minecraft.url = "github:Infinidoge/nix-minecraft";
    nix-minecraft.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    #rust-overlay = {
    #  url = "github:oxalica/rust-overlay";
    #  inputs.nixpkgs.follows = "nixpkgs";
    #};
  };

  outputs = {
    self,
    nixpkgs,
    agenix,
    nix-darwin,
    treefmt-nix,
    emacs-overlay,
    sops-nix,
    #rust-overlay,
    ...
  } @ inputs: let
    inherit (self) outputs;
    overlays = [
      #(import rust-overlay)
      emacs-overlay.overlay
    ];
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystemTypes = fn: nixpkgs.lib.genAttrs supportedSystems fn;
    conf = import ./config {};
    rootPath = ./.;
  in {
    nixosConfigurations = {
      minecraft = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs outputs nixpkgs rootPath conf;
          pkgs-forked = import inputs.nixpkgs-forked {
            inherit overlays;
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
        };
        modules = [./system/minecraft.nix];
      };
      raspberrypiimage = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = {
          inherit inputs outputs nixpkgs rootPath conf;
        };
        modules = [
          ./system/raspberrypiimage.nix
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
        ];
      };
      homelab = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = {
          inherit inputs outputs nixpkgs rootPath conf;
        };
        modules = [
          ./system/homelab
          agenix.nixosModules.default
          sops-nix.nixosModules.default
        ];
      };
    };
    darwinConfigurations = {
      bw = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = {
          inherit inputs outputs nixpkgs rootPath conf;
          hostname = "bw";
          pkgs-forked = import inputs.nixpkgs-forked {
            inherit overlays;
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
          pkgs = import inputs.nixpkgs {
            inherit overlays;
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
        };
        modules = [
          agenix.darwinModules.default
          sops-nix.darwinModules.default
          ./system/bw.nix
        ];
      };
      air = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = {
          inherit inputs outputs nixpkgs rootPath conf;
          hostname = "air";
          pkgs-forked = import inputs.nixpkgs-forked {
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
          pkgs = import inputs.nixpkgs {
            inherit overlays;
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
        };
        modules = [./system/air.nix];
      };
    };
    packages = forAllSystemTypes (system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      neovim =
        inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule
        {
          inherit pkgs;
          extraSpecialArgs = {inherit inputs outputs nixpkgs rootPath conf;};
          module = import ./system/with/user/with/program/neovim/nixvim.nix;
        };
    });
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
            pkgs.zstd
            (pkgs.writeScriptBin "update" ''
              nix flake update
            '')
            (pkgs.writeScriptBin "build-pi-image" ''
              nix build .#nixosConfigurations.raspberrypiimage.config.system.build.sdImage
            '')
            (pkgs.writeScriptBin "decompress-pi-image" ''
              zstd -d result/sd-image/*.img.zst -o result/sd-image/nixos-raspi.img
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
              treefmt --fail-on-change --no-cache
            fi
          '')
          (pkgs.writeScriptBin "apply" ''
            if [ "$1" == "formatting" ]; then
              echo "Applying formatting..."
              treefmt --no-cache
            fi
          '')
        ];
      };
      managing-secrets = pkgs.mkShell {
        packages = [
          agenix.packages.${system}.default
          sops-nix.packages.${system}.default
          pkgs.sops
          pkgs.yubikey-manager
          pkgs.gnupg
        ];
      };
      editing = pkgs.mkShell {
        packages = [
          pkgs.nixd
        ];
      };
      toggle-theme = let
        newSystemTheme =
          if conf.activeTheme.systemTheme == "dark"
          then {
            name = "light";
            darwinBool = "false";
          }
          else {
            name = "dark";
            darwinBool = "true";
          };
      in
        pkgs.mkShell {
          inputsFrom = with self.devShells.${system}; [
            building
          ];
          packages = [
            pkgs.neovim-remote
            (pkgs.writeScriptBin "nix-toggle-theme" ''
              cd ~/nix
              osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to ${newSystemTheme.darwinBool}' &
              echo "\"${newSystemTheme.name}\"" > "config/system-theme.nix"
              rebuild $1
              kill -SIGUSR1 $KITTY_PID &
              nvr --remote-silent --nostart -s -c "set background=${newSystemTheme.name}" &
            '')
            (pkgs.writeScriptBin "nix-set-colorscheme" ''
              cd ~/nix
              echo "\"$2\"" > "config/colorscheme.nix"
              rebuild $1
              kill -SIGUSR1 $KITTY_PID &
              nvr --remote-silent --nostart -s -c "colorscheme ${conf.activeTheme.colorScheme}" &
            '')
          ];
        };
    });
  };
}
