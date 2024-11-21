{
  inputs,
  pkgs,
  ...
}: {
  plugins.which-key.enable = false;
  plugins.barbecue.enable = false; # This is cool
  plugins.precognition.enable = false;
  plugins.web-devicons.enable = true;
  plugins.telescope.enable = true;
  plugins.telescope.extensions.file-browser.enable = true;
  plugins.telescope.extensions.file-browser.settings.hidden.file_browser =
    true;
  plugins.telescope.extensions.file-browser.settings.hidden.folder_browser =
    true;
  plugins.telescope.extensions.file-browser.settings.path = "%:p:h";
  plugins.telescope.extensions.file-browser.settings.select_buffer = true;
  plugins.telescope.settings.defaults.sorting_strategy = "ascending";
  plugins.telescope.settings.defaults.ignore_current_buffer = false;
  plugins.telescope.settings.defaults.sort_mru = true;
  plugins.telescope.settings.defaults.path_display = ["smart"];
  plugins.telescope.settings.defaults.layout_strategy = "vertical";
  plugins.telescope.settings.defaults.layout_config.width = 0.99;
  plugins.telescope.settings.defaults.layout_config.vertical.height = 0.99;
  plugins.telescope.settings.defaults.layout_config.vertical.mirror = true;
  plugins.telescope.settings.defaults.layout_config.vertical.prompt_position = "top";
  plugins.telescope.settings.defaults.layout_config.vertical.preview_height = 0.6;
  plugins.telescope.settings.defaults.layout_config.vertical.preview_cutoff = 0;
  plugins.telescope.settings.defaults.show_all_buffers = true;
  plugins.telescope.settings.defaults.cache_picker.num_pickers = 20;
  plugins.telescope.settings.defaults.cache_picker.ignore_empty_prompt = true;
  plugins.auto-session.enable = true;
  plugins.auto-session.settings.auto_create = true;
  plugins.auto-session.settings.auto_restore = true;
  plugins.auto-session.settings.auto_save = true;
  plugins.auto-session.settings.use_git_branch = true;
  plugins.auto-session.settings.suppressed_dirs = [
    "/"
    "~/"
    "~/Downloads"
  ];

  plugins.lsp.enable = true;
  plugins.lsp.servers.nixd.enable = true;
  plugins.lsp.servers.nixd.autostart = true;
  plugins.lsp.servers.nixd.cmd = ["nixd"];
  plugins.lsp.servers.csharp_ls.enable = true;
  plugins.lsp.servers.marksman.enable = true;
  plugins.lsp.servers.jsonls.enable = true;
  plugins.lsp.servers.marksman.settings.formatting.command = ["prettierd"];
  plugins.lsp.servers.ts_ls.enable = true;
  plugins.lsp.servers.eslint.enable = true;
  plugins.lsp.servers.sqls.enable = true;
  plugins.lsp.servers.rust_analyzer.enable = true;
  plugins.lsp.servers.lua_ls.enable = true;
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
  #plugins.fidget.enable = true;
  #plugins.fidget.notification.overrideVimNotify = true;
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
    {name = "nvim_lsp";}
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
  plugins.indent-blankline.enable = false;

  plugins.telescope.enabledExtensions = ["live_grep_args"];
  plugins.markdown-preview.enable = true;
  plugins.markdown-preview.settings.auto_close = 0;
  plugins.markdown-preview.settings.auto_start = 0;
  plugins.markdown-preview.settings.combine_preview = 1;
  plugins.markdown-preview.settings.echo_preview_url = 1;
  plugins.markdown-preview.settings.refresh_slow = 1;
  plugins.markdown-preview.settings.page_title = "$${name}";
  plugins.markdown-preview.settings.markdown_css = "${
    pkgs.writeText
    "markdown-preview.css"
    (builtins.readFile ./markdown-preview.css)
  }";

  plugins.treesitter.enable = true;
  plugins.treesitter.nixvimInjections = true;
  plugins.treesitter.settings.highlight.enable = true;
  plugins.treesitter.settings.incremental_selection.enable = false;
  plugins.treesitter.settings.indent.enable = false;

  plugins.zen-mode.enable = true;
  plugins.twilight.enable = true;

  # I really want to use this, but it seems to always do unexpected stuff.
  # Last time I enabled this it made the bottom row of the vim editor
  # blank.
  plugins.image = {
    enable = false;
    editorOnlyRenderWhenFocused = true;
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
        clearInInsertMode = true;
        onlyRenderImageAtCursor = true;
        downloadRemoteImages = true;
        filetypes = [
          "markdown"
          "vimwiki"
          "mdx"
        ];
      };
    };
  };

  plugins.render-markdown = {
    enable = false;
  };

  plugins.telescope.settings.pickers.buffers.mappings.i."<C-d>" = "delete_buffer";
  plugins.cmp.settings.mapping."<Right>" = "cmp.mapping.complete()";
  plugins.cmp.settings.mapping."<C-d>" = "cmp.mapping.scroll_docs(-4)";
  plugins.cmp.settings.mapping."<C-e>" = "cmp.mapping.close()";
  plugins.cmp.settings.mapping."<C-f>" = "cmp.mapping.scroll_docs(4)";
  plugins.cmp.settings.mapping."<CR>" = "cmp.mapping.confirm({ select = true })";
  plugins.cmp.settings.mapping."<Up>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
  plugins.cmp.settings.mapping."<Down>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
  plugins.telescope.settings.pickers.buffers.mappings.n."d" = "delete_buffer";

  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
    nvim-web-devicons
    telescope-live-grep-args-nvim
    #      (pkgs.vimUtils.buildVimPlugin {
    #        name = "bookmarks";
    # src = pkgs.fetchFromGitHub {
    #   owner = "addisonbeck";
    #   repo = "bookmarks.nvim";
    #   rev = "a798ff9a6af038641e02b74a47692b030947e64b";
    #   hash = "sha256-yGDOMHSPPrUxSLvZuS80yumsQEzJ2ha0IB48gL44tNs=";
    # };
    #        # src = builtins.fetchGit ./${config.home}/bookmarks.nvim;
    #      })
    (pkgs.vimUtils.buildVimPlugin {
      name = "satellite";
      src = pkgs.fetchFromGitHub {
        owner = "lewis6991";
        repo = "satellite.nvim";
        rev = "ea0a2e92bbb57981043fca334f5b274c0f279238";
        hash = "sha256-WVOYouiEFeLkQBe1Ptazw/mIfzxmaQmOuEK8KlfMYoQ=";
      };
    })
    inputs.where-am-i-nvim.packages.${pkgs.system}.default
  ];
}
