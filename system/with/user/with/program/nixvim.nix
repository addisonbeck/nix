# use z<Enter> z. and z- more!

{pkgs, ...}: {
  programs.nixvim = {
    enable = true;
    vimAlias = true;
    opts.background = "dark";
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
    opts.undofile = true;
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
    opts.cursorline = true;
    opts.wm = 2;
    opts.signcolumn = "yes";
    globals.netrw_banner = 0;
    plugins.web-devicons.enable = true;
    plugins.telescope.enable = true;
    plugins.telescope.extensions.file-browser.enable = true;
    plugins.telescope.extensions.file-browser.settings.hidden.file_browser =
      true;
    plugins.telescope.extensions.file-browser.settings.hidden.folder_browser =
      true;
    plugins.telescope.extensions.file-browser.settings.path = "%:p:h";
    plugins.telescope.extensions.file-browser.settings.select_buffer = true;
    plugins.telescope.settings.defaults.layout_strategy = "vertical";
    plugins.telescope.settings.defaults.sorting_strategy = "ascending";
    plugins.telescope.settings.defaults.ignore_current_buffer = false;
    plugins.telescope.settings.defaults.sort_mru = true;
    plugins.telescope.settings.defaults.path_display = ["smart"];
    plugins.telescope.settings.defaults.layout_config.width = 0.99;
    plugins.telescope.settings.defaults.layout_config.height = 0.99;
    plugins.telescope.settings.defaults.layout_config.mirror = true;
    plugins.telescope.settings.defaults.layout_config.prompt_position = "top";
    plugins.telescope.settings.defaults.layout_config.preview_height = 0.6;
    plugins.telescope.settings.defaults.show_all_buffers = true;
    plugins.telescope.settings.defaults.cache_picker.num_pickers = 20;
    plugins.telescope.settings.defaults.cache_picker.ignore_empty_prompt = true;

    extraConfigVim = ''
      set laststatus=0
      hi! link StatusLine Normal
      hi! link StatusLineNC Normal
      set statusline=%{repeat('─',winwidth('.'))}
    '';

    plugins.lsp.enable = true;
    plugins.lsp.servers.nil_ls.enable = true;
    plugins.lsp.servers.nil_ls.settings.formatting.command = ["nixfmt"];
    plugins.lsp.servers.csharp_ls.enable = true;
    plugins.lsp.servers.marksman.enable = true;
    plugins.lsp.servers.jsonls.enable = true;
    plugins.lsp.servers.marksman.settings.formatting.command = ["prettierd"];
    plugins.lsp.servers.ts_ls.enable = true;
    plugins.lsp.servers.eslint.enable = true;
    plugins.lsp.servers.sqls.enable = true;
    plugins.lsp.servers.rust_analyzer.enable = true;
    # Cargo should probably be installed by a devshell
    # Maybe vim should too
    plugins.lsp.servers.rust_analyzer.installCargo = false;
    plugins.lsp.servers.rust_analyzer.installRustc = false;
    plugins.flash.enable = true;
    plugins.flash.settings.jump.autojump = true;
    plugins.trouble.enable = false;
    plugins.trouble.settings.modes.diagnostics.auto_open = true;
    plugins.trouble.settings.modes.diagnostics.auto_close = true;
    plugins.trouble.settings.modes.lsp_document_symbols.auto_open = false;
    plugins.trouble.settings.modes.diagnostics.use_diagnostic_signs = true;
    plugins.trouble.settings.win.position = "right";
    plugins.trouble.settings.win.size.width = 60;
    plugins.noice.enable = false;
    plugins.fidget.enable = true;
    plugins.fidget.notification.overrideVimNotify = true;
    # local prettier = {
    #   formatCommand = 'prettierd "${INPUT}"',
    #   formatStdin = true,
    #   env = {
    #     string.format('PRETTIERD_DEFAULT_CONFIG=%s', vim.fn.expand('~/.config/nvim/utils/linter-config/.prettierrc.json')),
    #   },
    # }
    plugins.lsp.inlayHints = true;
    plugins.marks.enable = true;
    plugins.markview.enable = false;
    plugins.octo.enable = true;
    #plugins.nvim-web-devicons.enable = true;
    #plugins.plenary.enable = true;
    plugins.gitsigns.enable = true;
    plugins.gitlinker.enable = true;
    plugins.gitlinker.printUrl = false;
    plugins.lazygit.enable = true;

    plugins.cmp.enable = true;
    plugins.cmp.autoEnableSources = true;
    plugins.cmp.settings.sources = [
      { name = "nvim_lsp"; }
    ];
    plugins.cmp.settings.experimental.ghost_text = true;
    plugins.cmp.settings.performance.max_view_entries = 5;
    plugins.cmp.settings.window.completion.border = "rounded";
    plugins.cmp.settings.window.documentation.border = "rounded";
    plugins.cmp.settings.window.completion.col_offset = -3;
    plugins.cmp.settings.window.completion.side_padding = 0;
    plugins.cmp.settings.formatting.expandable_indicator = true;
    plugins.cmp.settings.performance.debounce = 60;
    plugins.cmp.settings.performance.fetching_timeout = 200;
    plugins.cmp.settings.completion.autocomplete = false;

    extraPlugins = with pkgs.vimPlugins; [
      plenary-nvim
      nvim-web-devicons
      telescope-live-grep-args-nvim
    ];
    plugins.telescope.enabledExtensions = ["live_grep_args"];
    plugins.markdown-preview.enable = true;

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
    plugins.treesitter.nixvimInjections = true;
    plugins.treesitter.settings.highlight.enable = true;
    plugins.treesitter.settings.incremental_selection.enable = false;
    plugins.treesitter.settings.indent.enable = false;

    plugins.zen-mode.enable = true;
    plugins.twilight.enable = true;

    # This does not work well, but is cool
    plugins.image = {
      enable = false;
      backend = "kitty";
      hijackFilePatterns = [
        "*.png"
        "*.jpg"
        "*.jpeg"
        "*.gif"
        "*.webp"
      ];
      maxHeightWindowPercentage = 25;
      tmuxShowOnlyInActiveWindow = true;
      integrations = {
        markdown = {
          enabled = true;
          downloadRemoteImages = true;
          filetypes = [
            "markdown"
            "vimwiki"
            "mdx"
          ];
        };
      };
    };

    keymaps = [
      {
        action = ":";
        key = "<Space>";
        mode = ["n" "v"];
        options = {
          desc = "Map space to :";
          silent = true;
        };
      }
      {
        action = "<C-d>";
        key = "<S-j>";
        mode = ["n" "v"];
        options = {
          desc = "Jump down the page";
          silent = true;
        };
      }
      {
        action = "<C-u>";
        key = "<S-k>";
        mode = ["n" "v"];
        options = {
          desc = "Jump down the page";
          silent = true;
        };
      }
      {
        action = ":set relativenumber! nu!<CR>";
        key = "<S-n>";
        mode = "n";
        options = {
          desc = "Toggle line numbers";
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
        mode = ["n" "x" "o"];
        key = "s";
        action = "<cmd>lua require('flash').jump()<cr>";
        options = {desc = "Flash";};
      }
      {
        mode = ["n"];
        key = "yg";
        action = ''
          <cmd>lua require"gitlinker".get_buf_range_url("n", {action_callback = require"gitlinker.actions".copy_to_clipboard})<cr>'';
        options = {
          desc = "Copy URL of current line on GitHub";
          silent = true;
        };
      }
      {
        mode = ["v"];
        key = "yg";
        action = ''<cmd>lua require"gitlinker".get_buf_range_url("v")<cr>'';
        options = {
          desc = "Copy URL of current line on GitHub";
          silent = true;
        };
      }
      {
        mode = ["n"];
        key = "<Up>";
        action = "<cmd>B<cr>";
        options = {
          desc = "Go to a open buffer";
          silent = true;
        };
      }
      {
        mode = ["n"];
        key = "<Down>";
	# Combine diagnostics?
        action = "<cmd>SearchMarks<cr>";
        options = {
          desc = "Go to a mark";
          silent = true;
        };
      }
      {
        mode = ["n"];
        key = "<Left>";
        action = '':bprevious<cr>:ls<cr>'';
        options = {
          desc = "Go to previous buffer";
          silent = true;
        };
      }
      {
        mode = ["n"];
        key = "<Tab>";
	# This will swap to the previous buffer in pure vim. Neat!
        # action = '':b#<cr>'';
	action = ''<cmd>G<cr>'';
        options = {
          desc = "Search Git files";
          silent = true;
        };
      }
      {
        mode = ["n"];
        key = "<S-Tab>";
	action = ''<cmd>F<cr>'';
        options = {
          desc = "Search file tree";
          silent = true;
        };
      }
    ];
    plugins.telescope.settings.pickers.buffers.mappings.i."<C-d>" = "delete_buffer";
    plugins.cmp.settings.mapping."<Right>" = "cmp.mapping.complete()";
    plugins.cmp.settings.mapping."<C-d>" = "cmp.mapping.scroll_docs(-4)";
    plugins.cmp.settings.mapping."<C-e>" = "cmp.mapping.close()";
    plugins.cmp.settings.mapping."<C-f>" = "cmp.mapping.scroll_docs(4)";
    plugins.cmp.settings.mapping."<CR>" = "cmp.mapping.confirm({ select = true })";
    plugins.cmp.settings.mapping."<Up>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
    plugins.cmp.settings.mapping."<Down>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
    plugins.telescope.settings.pickers.buffers.mappings.n."d" = "delete_buffer";
   
    userCommands."SearchNotes".command = "lua require('telescope.builtin').live_grep({ cwd = '~/notes' })";
    userCommands."GrepWord".command = "lua require('telescope-live-grep-args.shortcuts').grep_word_under_cursor({ search_dirs = {'.', '~/notes/'} })";

    # Pure vim implementation
    # userCommands."SearchMarks".command = "marks abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<cr>:'
    userCommands."SearchMarks".command = "lua require('telescope.builtin').marks()";
    userCommands."SearchDiagnostics".command = "lua require('telescope.builtin').diagnostics()";
    userCommands."R".command = "lua require('telescope.builtin').resume()";
    userCommands."P".command = "lua require('telescope.builtin').pickers()";
    userCommands."RenameCurrentFile".command = "lua vim.lsp.buf.rename()";
    userCommands."Spellcheck".command = "lua require('telescope.builtin').spell_suggest()";
    userCommands."CodeAction".command = "lua vim.lsp.buf.code_action()";
    userCommands."G".command = "lua require('telescope.builtin').git_files()";
    # Pure Vim implementation
    # userCommands."B".command = "ls<cr>:b<space>";
    userCommands."B".command = "lua require('telescope.builtin').buffers()";
    userCommands."Oldfiles".command = "lua require('telescope.builtin').oldfiles()";
    # Use :Ex to do this with pure vim
    userCommands."F".command = "lua require('telescope').extensions.file_browser.file_browser()";
    userCommands."D".command = "lua require('telescope.builtin').lsp_definitions()";
    userCommands."SearchDefinitions".command = "lua require('telescope.builtin').lsp_definitions()";
    userCommands."SearchReferences".command = "lua require('telescope.builtin').lsp_references()";
    userCommands."SearchImplementations".command = "lua require('telescope.builtin').lsp_implementations()";
    userCommands."Format".command = "lua vim.lsp.buf.format()";
    userCommands."CopyRelativePath".command = "let @+ = expand('%:p:.')";
    userCommands."CopyFullPath".command = "let @+ = expand('%:p')";
    userCommands."CopyFileName".command = "let @+ = expand('%:t')";
    userCommands."GenerateGuid".command = "silent! read !uuidgen";
    userCommands."Bd".command = "silent! execute '%bd|e#|bd#'";
    highlight = {
      # "Incandescent Light Bulb
      ActiveYank.bg = "#FFBB73";
      ActiveYank.fg = "#000000";
    };
    autoCmd = [
      {
        event = [
          "FileType"
        ];
        pattern = [
	  "qf"
          "help"
	  "man"
	  "lspinfo"
        ];
        callback = { 
	  __raw = ''
	    function()
	      vim.cmd([[
	        set buflisted
		wincmd o
	      ]])
	    end
	''; 
	};
      }
      {
        event = [
          "FileType"
        ];
        pattern = [
	  "md"
	  "markdown"
          "txt"
	  "gitcommit"
        ];
        callback = { 
	  __raw = ''
	    function()
	      vim.opt_local.wrap = true
	    end
	''; 
	};
      }
      {
        event = [ "TextYankPost" ];
        pattern = [ "*" ];
        command = ''lua vim.highlight.on_yank({higroup="ActiveYank", timeout=300})'';
      }
    ];
  };
}
