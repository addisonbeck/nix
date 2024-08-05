  {
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixvim,
    agenix,
    nix-darwin,
    ...
  } @ inputs: let
    inherit (self) outputs;
    supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
      inherit system;
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
    darwinConfigurations = {
      # nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake .#bw
      bw = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = { inherit inputs; };
        modules = [ ./system/bw.nix ];
      };
    };
    devShells = forEachSupportedSystem ({ pkgs, nixvim, system }: {
      default = pkgs.mkShell {
        packages = [
          (nixvim.makeNixvim {
	    vimAlias = true;
	    colorschemes.gruvbox.enable = true;
	    colorschemes.gruvbox.settings.transparent_mode = true;
	    colorschemes.gruvbox.settings.overrides = {
	      Winbar = {
		bold = true;
		fg = 4;
		bg = "NONE";
	      };
	      WinbarNC = {
		bold = true;
		fg = 8;
		bg = "NONE";
	      };
	    };
	    opts.termguicolors = false;
	    plugins.telescope.enable = true;
	    plugins.telescope.extensions.file-browser.enable = true;
	    plugins.telescope.extensions.file-browser.settings.hidden.file_browser = true;
	    plugins.telescope.extensions.file-browser.settings.hidden.folder_browser = true;

	    extraConfigVim = ''
	      set laststatus=0
              hi! link StatusLine Normal
              hi! link StatusLineNC Normal
              set statusline=%{repeat('─',winwidth('.'))}
	    '';

	    plugins.lsp.enable = true;
            plugins.lsp.servers.nil-ls.enable = true;

	    plugins.treesitter.enable = true;
            plugins.treesitter.nixvimInjections = true;
            plugins.treesitter.settings.highlight.enable = true;
            plugins.treesitter.settings.incremental_selection.enable = true;
            plugins.treesitter.settings.indent.enable = true;
	    
	    keymaps = [
              {
                action = ":Telescope file_browser<CR>";
                key = "\\";
                options = {
		  desc = "Open a file browser";
                  silent = true;
                };
		mode = "n";
              }
              {
                action = ":Telescope<CR>";
                key = "|";
                options = {
		  desc = "Search through Telescope pickers";
                  silent = true;
                };
		mode = "n";
              }
              {
                action = ":Telescope buffers<CR>";
                key = "<Tab>";
                options = {
		  desc = "Search through open buffers";
                  silent = true;
                };
		mode = "n";
              }
              {
                action = ":Telescope git_files<CR>";
                key = "<S-Tab>";
                options = {
		  desc = "Search through files in the active git repository";
                  silent = true;
                };
		mode = "n";
              }
	      {
	        action = "<C-d>";
		key = "<S-j>";
                options = {
		  desc = "Jump down the page";
                  silent = true;
                };
		mode = "n";
	      }
	      {
	        action = "<C-u>";
		key = "<S-k>";
                options = {
		  desc = "Jump down the page";
                  silent = true;
                };
		mode = "n";
	      }
	      {
	        action = ":set nu!<CR>";
		key = "<S-n>";
                options = {
		  desc = "Toggle line numbers";
                  silent = true;
                };
		mode = "n";
	      }
           ];
          })
          pkgs.lazygit 
	  agenix.packages.${system}.default
        ];
      };
    });
  };
}
