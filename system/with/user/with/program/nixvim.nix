{ pkgs, ... }: {
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
    opts.signcolumn = "no";
    plugins.telescope.enable = true;
    plugins.telescope.extensions.file-browser.enable = true;
    plugins.telescope.extensions.file-browser.settings.hidden.file_browser =
      true;
    plugins.telescope.extensions.file-browser.settings.hidden.folder_browser =
      true;
    plugins.telescope.extensions.file-browser.settings.path = "%:p:h";
    plugins.telescope.settings.defaults.layout_strategy = "vertical";
    plugins.telescope.settings.defaults.sorting_strategy = "ascending";
    plugins.telescope.settings.defaults.ignore_current_buffer = false;
    plugins.telescope.settings.defaults.sort_mru = true;
    plugins.telescope.settings.defaults.path_display = [ "smart" ];
    plugins.telescope.settings.defaults.layout_config.width = 0.99;
    plugins.telescope.settings.defaults.layout_config.height = 0.99;
    plugins.telescope.settings.defaults.layout_config.mirror = true;
    plugins.telescope.settings.defaults.layout_config.prompt_position = "top";
    plugins.telescope.settings.defaults.layout_config.preview_height = 0.6;
    plugins.telescope.settings.defaults.show_all_buffers = true;

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
    plugins.lsp.servers.marksman.enable = true;
    plugins.lsp.servers.marksman.settings.formatting.command = [ "prettierd" ];
    # local prettier = {
    #   formatCommand = 'prettierd "${INPUT}"',
    #   formatStdin = true,
    #   env = {
    #     string.format('PRETTIERD_DEFAULT_CONFIG=%s', vim.fn.expand('~/.config/nvim/utils/linter-config/.prettierrc.json')),
    #   },
    # }
    plugins.lsp.inlayHints = true;
    extraConfigLua = ''
        vim.diagnostic.config({
          virtual_text = {
            prefix = "",
            spacing = 0,
            format = function(diagnostic)
              if diagnostic.severity == vim.diagnostic.severity.ERROR then
                return '←🧚'
              end
              if diagnostic.severity == vim.diagnostic.severity.WARN then
                return '←🧚'
              end
              if diagnostic.severity == vim.diagnostic.severity.INFO then
                return '←🧚'
              end
              if diagnostic.severity == vim.diagnostic.severity.HINT then
                return '←🧚'
              end
              return diagnostic.message
            end
          },
        })
        vim.api.nvim_set_hl(0, "@markup.heading", {
      	  underdotted = true,
      	  bold = true,
      	  italic = true,
      	})
        vim.api.nvim_set_hl(0, "@markup.quote.markdown", {
        italic = true,
      })
    '';
    diagnostics = {
      signs = false;
      underline = true;
      update_in_insert = false;
      float = {
        focused = false;
        style = "minimal";
        border = "rounded";
        source = "always";
        header = "";
        prefix = "";
      };
    };

    plugins.treesitter.enable = true;
    plugins.treesitter.grammarPackages =
      [ pkgs.vimPlugins.nvim-treesitter-parsers.nix ];
    plugins.treesitter.nixvimInjections = true;
    plugins.treesitter.settings.highlight.enable = true;
    plugins.treesitter.settings.incremental_selection.enable = false;
    plugins.treesitter.settings.indent.enable = false;

    plugins.zen-mode.enable = true;
    plugins.twilight.enable = true;

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
      {
        action = "<cmd>lua vim.lsp.buf.code_action()<CR>";
        key = "DA";
        mode = "n";
        options = {
          desc = "Perform a code action";
          silent = true;
        };
      }
      {
        action = "<cmd>lua vim.diagnostic.go_to_next()<CR>";
        key = "DJ";
        mode = "n";
        options = {
          desc = "Go to next diagnostic";
          silent = true;
        };
      }
      {
        action = "<cmd>lua vim.diagnostic.go_to_previous()<CR>";
        key = "DK";
        mode = "n";
        options = {
          desc = "Go to previous diagnostic";
          silent = true;
        };
      }
      {
        action = "<cmd>lua vim.lsp.buf.rename()<CR>";
        key = "RN";
        mode = "n";
        options = {
          desc = "Rename the current file";
          silent = true;
        };
      }
      {
        action = "<cmd>lua require('telescope.builtin').spell_suggest()<CR>";
        key = "S=";
        mode = "n";
        options = {
          desc = "Spell check";
          silent = true;
        };
      }
      {
        action =
          "<cmd>lua require('telescope.builtin').live_grep({ cwd = '~/notes' })<CR>";
        key = "SN";
        mode = "n";
        options = {
          desc = "Search notes";
          silent = true;
        };
      }
      {
        action = "<cmd>lua require('telescope.builtin').live_grep()<CR>";
        key = "SG";
        mode = "n";
        options = {
          desc = "Search notes";
          silent = true;
        };
      }
      {
        action = "<cmd>lua require('telescope.builtin').lsp_definitions()<CR>";
        key = "SDE";
        mode = "n";
        options = {
          desc = "Search definitions for the symbol under the cursor";
          silent = true;
        };
      }
      {
        action = "<cmd>lua require('telescope.builtin').diagnostics()<CR>";
        key = "SDI";
        mode = "n";
        options = {
          desc = "Search availible diagnostics";
          silent = true;
        };
      }
      {
        action =
          "<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>";
        key = "SS";
        mode = "n";
        options = {
          desc = "Search all symbols in the current document";
          silent = true;
        };
      }
      {
        action =
          "<cmd>lua require('telescope.builtin').lsp_implementations()()<CR>";
        key = "SI";
        mode = "n";
        options = {
          desc = "Search implementations for the symbol under the cursor";
          silent = true;
        };
      }
      {
        action = "<cmd>lua require('telescope.builtin').lsp_references()<CR>";
        key = "SR";
        mode = "n";
        options = {
          desc = "Search references for the symbol under the cursor";
          silent = true;
        };
      }
      {
        action = "<cmd>ZenMode<CR>";
        key = "Z";
        mode = "n";
        options = {
          desc = "Toggle Zen Mode";
          silent = true;
        };
      }
    ];

    userCommands.CopyFileName.command = "let @+ = expand('%')";
  };
}