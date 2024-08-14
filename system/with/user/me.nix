{
  inputs,
  pkgs,
  lib,
  ...
}: {
  users.users.me = {
    shell = pkgs.zsh;
  } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    name = "me";
    home = "/Users/me";
  } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
    users.users.me.isNormalUser = true;
    users.users.me.initialPassword = "me";
    users.users.me.extraGroups = [ "wheel" "docker" ];
  };

  home-manager.users.me = {
    imports = [ 
      inputs.stylix.homeManagerModules.stylix
      inputs.agenix.homeManagerModules.default
      inputs.nixvim.homeManagerModules.default
      ./with/trait/well-known-hosts.nix
      ./with/program/bash.nix
      ./with/program/git.nix
      ./with/program/direnv.nix
      ./with/program/tmux.nix
      ./with/program/kitty.nix
      ./with/secret/github.nix
      ./with/service/autoclone.nix
      { services.autoclone.enable = true; }
      ./with/development-environment/notes
      ./with/development-environment/bitwarden
    ];

    stylix.enable = true;
    stylix.image = ./with/wallpaper/empty.png;
    stylix.base16Scheme =
      "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
    stylix.fonts = {
      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };

      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };

      monospace = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans Mono";
      };

      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };
    stylix.fonts.sizes.terminal = 16;
    stylix.opacity.terminal = 0.95;
    stylix.targets.nixvim.transparentBackground.main = true;
    programs.zsh.enable = true;
    home.sessionPath = [
      "/Users/me/bin/binwarden"
      "/Users/me/bin"
      "/opt/homebrew/bin"
      "/opt/homebrew/sbin"
    ];
    programs.zsh.profileExtra = ''
      export NVM_DIR="$HOME/.nvm"
      [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    '';
    home.stateVersion = "24.05";
    home.enableNixpkgsReleaseCheck = false;
    home.packages = [
      pkgs.nixfmt
      pkgs.dotnet-sdk_8
      pkgs.raycast
      (pkgs.writeShellScriptBin "nuke-docker" ''
        	docker stop $(docker ps -a -q)
        	docker rm $(docker ps -a -q)
        	docker volume rm $(docker volume ls -q)
                docker network prune
      '')
    ];
    programs.direnv.config.whitelist.exact = [ "/Users/me/nix" ];
    launchd.agents.raycast = {
      enable = true;
      config = {
        ProgramArguments =
          [ "${pkgs.raycast}/Applications/Raycast.app/Contents/MacOS/Raycast" ];
        KeepAlive = true;
        RunAtLoad = true;
      };
    };

    programs.lazygit.enable = true;
    programs.nixvim = {
      enable = true;
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
      opts.autoindent = true;
      opts.smartindent = false;
      opts.confirm = false;
      opts.swapfile = false;
      opts.wrap = false;
      opts.clipboard = "unnamed";
      opts.cmdheight = 0;
      opts.hidden = true;
      opts.ignorecase = true;
      opts.lazyredraw = true;
      opts.mouse = "a";
      opts.scrolljump = 5;
      opts.showmode = false;
      opts.smartcase = true;
      opts.splitbelow = true;
      opts.splitright = true;
      opts.updatetime = 300;
      opts.spell = true;
      opts.spelllang = "en_us";
      opts.textwidth = 77;
      opts.wm = 2;
      plugins.telescope.enable = true;
      plugins.telescope.extensions.file-browser.enable = true;
      plugins.telescope.extensions.file-browser.settings.hidden.file_browser =
        true;
      plugins.telescope.extensions.file-browser.settings.hidden.folder_browser =
        true;
      plugins.telescope.extensions.file-browser.settings.path = "%:p:h";
      plugins.telescope.settings.pickers.buffers.layout_strategy = "vertical";
      plugins.telescope.settings.pickers.buffers.sorting_strategy = "ascending";
      plugins.telescope.settings.pickers.buffers.ignore_current_buffer = false;
      plugins.telescope.settings.pickers.buffers.sort_mru = true;
      plugins.telescope.settings.pickers.buffers.path_display = [ "smart" ];
      plugins.telescope.settings.pickers.buffers.layout_config.width = 0.99;
      plugins.telescope.settings.pickers.buffers.layout_config.height = 0.99;
      plugins.telescope.settings.pickers.buffers.layout_config.mirror = true;
      plugins.telescope.settings.pickers.buffers.layout_config.prompt_position = "top";
      plugins.telescope.settings.pickers.buffers.layout_config.preview_height = 0.6;
      plugins.telescope.settings.pickers.buffers.show_all_buffers = true;

      extraConfigVim = ''
        set laststatus=0
        hi! link StatusLine Normal
        hi! link StatusLineNC Normal
        set statusline=%{repeat('─',winwidth('.'))}
      '';

      plugins.lsp.enable = true;
      plugins.lsp.servers.nil-ls.enable = true;
      plugins.lsp.servers.nil-ls.settings.formatting.command = [ "nixfmt" ];
      plugins.lsp.servers.csharp-ls.enable = true;

      plugins.treesitter.enable = true;
      plugins.treesitter.grammarPackages =
        [ pkgs.vimPlugins.nvim-treesitter-parsers.nix ];
      plugins.treesitter.nixvimInjections = true;
      plugins.treesitter.settings.highlight.enable = true;
      plugins.treesitter.settings.incremental_selection.enable = false;
      plugins.treesitter.settings.indent.enable = false;

      keymaps = [
        {
          action = ":Telescope file_browser<CR>";
          key = "\\";
          mode = "n";
          options = {
            desc = "Open a file browser";
            silent = true;
          };
        }
        {
          action = ":Telescope<CR>";
          key = "|";
          mode = "n";
          options = {
            desc = "Search through Telescope pickers";
            silent = true;
          };
        }
        {
          action = "<cmd>lua require('telescope.builtin').buffers()<cr>";
          key = "<Tab>";
          mode = "n";
          options = {
            desc = "Search through open buffers";
            silent = true;
          };
        }
        {
          action = ":Telescope git_files<CR>";
          key = "<S-Tab>";
          mode = "n";
          options = {
            desc = "Search through files in the active git repository";
            silent = true;
          };
        }
        {
          action = "<C-d>";
          key = "<S-j>";
          mode = "n";
          options = {
            desc = "Jump down the page";
            silent = true;
          };
        }
        {
          action = "<C-u>";
          key = "<S-k>";
          mode = "n";
          options = {
            desc = "Jump down the page";
            silent = true;
          };
        }
        {
          action = ":set nu!<CR>";
          key = "<S-n>";
          mode = "n";
          options = {
            desc = "Toggle line numbers";
            silent = true;
          };
        }
        {
          action = "<cmd>lua vim.lsp.buf.format()<CR>";
          key = "FF";
          mode = "n";
          options = {
            desc = "Format the file";
            silent = true;
          };
        }
	{
          action = "<cmd>lua vim.lsp.buf.hover()<CR>";
          key = "HH";
          mode = "n";
          options = {
            silent = true;
            desc = "Hover";
          };
        }
        {
          action = "<cmd>lua vim.diagnostic.open_float()<CR>";
          key = "DD";
          mode = "n";
          options = {
            silent = true;
            desc = "Open diagnostic";
          };
        }
        {
          action = "<cmd>wincmd h<CR>";
          key = "<c-h>";
          mode = "n";
          options = {
            desc = "Navigate splits";
            silent = true;
          };
        }
        {
          action = "<cmd>wincmd j<CR>";
          key = "<c-j>";
          options = {
            desc = "Navigate splits";
            silent = true;
          };
        }
        {
          action = "<cmd>wincmd k<CR>";
          key = "<c-k>";
          options = {
            desc = "Navigate splits";
            silent = true;
          };
        }
        {
          action = "<cmd>wincmd l<CR>";
          key = "<c-l>";
          mode = "n";
          options = {
            desc = "Navigate splits";
            silent = true;
          };
        }
      ];
    };
  };
}
