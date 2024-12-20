# use z<Enter> z. and z- more!
{
  pkgs,
  lib,
  inputs,
  conf,
  ...
}: let
  options =
    (
      import ./options {}
    )
    // (
      (import ./colors {inherit conf;}).options
    );
in {
  vimAlias = true;
  opts = options;
  colorschemes = (import ./colors {inherit conf;}).colorscheme;
  highlight = (import ./colors {inherit conf;}).highlight;
  plugins = (import ./plugins {inherit inputs pkgs;}).plugins;
  extraPlugins = (import ./plugins {inherit inputs pkgs;}).extraPlugins;
  autoCmd = (import ./auto-commands {inherit conf;}).autoCommands;
  keymaps =
    [
      # Custom nixvim style keymaps can be added here if needed, but I
      # stick to using `mkVimKeymaps` and the `commands` data structure it
      # references.
    ]
    ++ (import ./commands {inherit lib;}).keymaps;
  userCommands =
    {
      # Custom nixvim style commands can be added here if needed, but I
      # stick to using `mkVimUserCommand` and the `commands` data structure
      # it references.
    }
    // (import ./commands {inherit lib;}).userCommands;
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
  globals.netrw_banner = 0;
  extraConfigVim = ''
    set laststatus=0
    hi! link StatusLine Normal
    hi! link StatusLineNC Normal
    set statusline=%{repeat('─',winwidth('.'))}
  '';
  extraConfigLua = ''
         require('where-am-i').setup({
           features = {
             user_commands = {
               enable = true;
             },
             keymaps = {
               enable = true;
             },
           },
           display = {
             style = {
               border = "single",
             },
           },
         })
    --      require('satellite').setup({
    --     current_only = false,
    --      winblend = 0,
    --      zindex = 40,
    --      excluded_filetypes = {},
    --      width = 2,
    --      handlers = {
    --        cursor = {
    --          enable = true,
    --          -- Supports any number of symbols
    --          symbols = { '⎺', '⎻', '⎼', '⎽' }
    --          -- symbols = { '⎻', '⎼' }
    --          -- Highlights:
    --          -- - SatelliteCursor (default links to NonText
    --        },
    --        search = {
    --          enable = true,
    --          -- Highlights:
    --          -- - SatelliteSearch (default links to Search)
    --          -- - SatelliteSearchCurrent (default links to SearchCurrent)
    --        },
    --        diagnostic = {
    --          enable = true,
    --          signs = {'-', '=', '≡'},
    --          min_severity = vim.diagnostic.severity.HINT,
    --          -- Highlights:
    --          -- - SatelliteDiagnosticError (default links to DiagnosticError)
    --          -- - SatelliteDiagnosticWarn (default links to DiagnosticWarn)
    --          -- - SatelliteDiagnosticInfo (default links to DiagnosticInfo)
    --          -- - SatelliteDiagnosticHint (default links to DiagnosticHint)
    --        },
    --        gitsigns = {
    --          enable = true,
    --          signs = { -- can only be a single character (multibyte is okay)
    --     add = "│",
    --     change = "│",
    --     delete = "-",
    --          },
    --          -- Highlights:
    --          -- SatelliteGitSignsAdd (default links to GitSignsAdd)
    --          -- SatelliteGitSignsChange (default links to GitSignsChange)
    --          -- SatelliteGitSignsDelete (default links to GitSignsDelete)
    --        },
    --        marks = {
    --          enable = true,
    --          show_builtins = false, -- shows the builtin marks like [ ] < >
    --          key = 'm'
    --          -- Highlights:
    --          -- SatelliteMark (default links to Normal)
    --        },
    --        quickfix = {
    --          signs = { '-', '=', '≡' },
    --          -- Highlights:
    --          -- SatelliteQuickfix (default links to WarningMsg)
    --        }
    --      },
    --      });
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
  '';
}
