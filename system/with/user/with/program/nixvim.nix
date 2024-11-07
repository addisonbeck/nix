# use z<Enter> z. and z- more!
{
  pkgs,
  lib,
  ...
}: let
  # Command definitions are written as raw lua functions to force the
  # `desc` property to apply and appear in commands like :command. If a
  # simple string as passed as the command definition that is always used
  # instead of `desc`. Neovim upstream doesn't currently have any plans for
  # a true description field for commands 😔
  #
  # I've gotten into some nasty error messages writing my commands this
  # way. Rebuilding the config will say something like
  #
  # ```
  # unexpected token # '{'
  # ```
  #
  # The error will appear to be tied to a specific user command, but really
  # it will just show the first command defined in alphabetical order.
  #
  # The issue will really be related to a random defined command. I've
  # bumped into this by:
  #
  # - Leaving a `lua` call in front of a bit of lua code from converting it
  #   to a function.
  commands = {
    # TODO: Implement this command that will toggle back and forth between
    # the last two buffers:
    # ```
    # action = '':b#<cr>'';
    # ```
    searchGitFiles = {
      description = ''
        Search files in the current working directory's git repository
      '';
      # TODO: Add aliases or multiple vim command definitions for a single
      # command structure.
      vimCommandName = "SearchGitFiles";
      vimKeymapBinding =
        {
          modes = ["n" "v" "i"];
          key = "<C-g>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').git_files()
        end
      '';
    };
    searchDefinitions = {
      description = ''
        Search lsp definitions of the symbol under the cursor
      '';
      vimCommandName = "SearchDefintions";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-d>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').lsp_definitions()
        end
      '';
    };
    searchMarks = {
      description = ''
        Search for marks with telescope.
      '';
      vimCommandName = "SearchMarks";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-m>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').marks();
        end
      '';
    };
    searchNotes = {
      description = ''Grep search my notes'';
      vimCommandName = "SearchNotes";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-n>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').live_grep({ cwd = '~/notes' });
        end
      '';
    };
    liveGrepWordWithNotes = {
      description = ''
        Grep search the current directory. Also includes the ~/notes directory
      '';
      vimCommandName = "SearchLiveGrep";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-l>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          local picker = require('telescope-live-grep-args.shortcuts');
          picker.grep_word_under_cursor({
            search_dirs = {'.', '~/notes/'}
          });
        end
      '';
    };
    resumeLastOpenedTelescopePicker = {
      description = ''
        Resume the last opened telescope picker
      '';
      vimCommandName = "SearchResume";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-r>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').resume();
        end
      '';
    };
    searchTelescopePickers = {
      description = ''
        Search telescope pickers
      '';
      vimCommandName = "SearchTelescopePickers";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-\>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').pickers();
        end
      '';
    };
    renameCurrentFile = {
      description = ''
        Rename the currently open file
      '';
      vimCommandName = "RenameCurrentFile";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-.>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.lsp.buf.rename();
        end
      '';
    };
    spellCheck = {
      description = ''
        Open spell suggest for the symbol under the cursor
      '';
      vimCommandName = "Spellcheck";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          # I'd rather this be mapped to save all quit
          key = "<C-q>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').spell_suggest()
        end
      '';
    };
    runCodeAction = {
      description = ''
        Open code action suggestions for the diagnostic under the cursor
      '';
      vimCommandName = "RunCodeAction";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-a>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.lsp.buf.code_action()
        end
      '';
    };
    searchBuffers = {
      description = ''
        Search through the currently open vim buffers
      '';
      vimCommandName = "SearchBuffers";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-b>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').buffers()
        end
      '';
    };
    searchOldfiles = {
      description = ''
        Search through vim's oldfiles
      '';
      vimCommandName = "SearchOldfiles";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-o>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').oldfiles();
        end
      '';
    };
    searchFileTree = {
      description = ''
        Open a file tree browser
      '';
      vimCommandName = "SearchFileTree";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-/>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope').extensions.file_browser.file_browser();
        end
      '';
    };
    searchLspReferences = {
      description = ''
        Search lsp references of the word under the cursor
      '';
      vimCommandName = "SearchLspReferences";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-1>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').lsp_references();
        end
      '';
    };
    searchLspImplementations = {
      description = ''
        Search lsp implementations of the symbol under the cursor
      '';
      vimCommandName = "SearchLispImplementations";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-2>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').lsp_implementations();
        end
      '';
    };
    formatBuffer = {
      description = ''
        Format the open buffer
      '';
      vimCommandName = "FormatBuffer";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-f>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.lsp.buf.format();
        end
      '';
    };
    copyRelativePath = {
      description = ''
        Copy the relative path (compared to the current open directory) of
        the file loaded in the open buffer
      '' ;
      vimCommandName = "CopyRelativePath";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-p>r";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.cmd.normal('let @+ = expand(%:p:.\')')
        end
      '';

    };
    copyFullPath = {
      description = ''
        Copy the full system path of the file loaded in the open buffer
      '';
      vimCommandName = "CopyFullPath";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-p>f";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.cmd.normal('let @+ = expand(\'%:p\')')
        end
      '';
    };
    copyFileName = {
      description = ''
        Copy the file name of the file loaded in the open buffer
      '';
      vimCommandName = "CopyFileName";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-p>n";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.cmd.normal('let @+ = expand(\'%:t\')')
        end
      '';
    };
    generateGuid = {
      description = ''
        Generate a guid and paste it under the cursor
      '';
      vimCommandName = "GenerateGuid";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-g>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.cmd.normal('silent! read !uuidgen')
        end
      '';
    };
    closeOtherBuffers = {
      description = ''
        Close all open buffers except for the currently loaded one
      '';
      vimCommandName = "Bd";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-->";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          vim.cmd.normal('silent! execute \'%bd|e#|bd#')
        end
      '';
    };
    searchLspDiagnostics = {
      description = ''
        Search lsp diagnostics
      '';
      vimCommandName = "SearchLspDiagnostics";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-0>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          require('telescope.builtin').diagnostics()
        end
      '';
    };
    searchKeymaps = {
      description = ''
        Search registered keymaps
      '';
      vimCommandName = "SearchKeymaps";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-k>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          local pickers = require('telescope.pickers')
          local finders = require('telescope.finders')
          local actions = require('telescope.actions')
          local action_state = require('telescope.actions.state')
          local make_entry = require "telescope.make_entry"
          local entry_display = require "telescope.pickers.entry_display"
          local sorters = require('telescope.sorters')
          local previewers = require('telescope.previewers')
          local utils = require "telescope.utils"
          local conf = require("telescope.config").values

          local keymap_encountered = {} -- used to make sure no duplicates are inserted into keymaps_table
          local keymaps_table = {}
          local max_len_lhs = 0

          -- helper function to populate keymaps_table and determine max_len_lhs
          local function extract_keymaps(keymaps)
            for _, keymap in pairs(keymaps) do
              local keymap_key = keymap.buffer .. keymap.mode .. keymap.lhs -- should be distinct for every keymap
              if not keymap_encountered[keymap_key] then
                keymap_encountered[keymap_key] = true
                table.insert(keymaps_table, keymap)
                max_len_lhs = math.max(max_len_lhs, #utils.display_termcodes(keymap.lhs))
              end
            end
          end
          for _, mode in pairs({ "n", "i", "c", "x" }) do
            local global = vim.api.nvim_get_keymap(mode)
            local buf_local = vim.api.nvim_buf_get_keymap(0, mode)
            extract_keymaps(global)
            extract_keymaps(buf_local)
          end
            pickers.new({
              layout_strategy = "cursor",
              layout_config = {
                height = 0.4,
                width = 0.6,
              },
              preview = {
                wrap = true,
              },
              previewer = previewers.new_buffer_previewer ({
                define_preview = function(self, entry, status)
                  local lines = {}
                  if entry.definition ~= nil then
                    for line in string.gmatch(entry.definition, "([^\n]*)") do
                      table.insert(lines, line)
                    end
                    vim.api.nvim_buf_set_lines(self.state.bufnr, 0, -1, false, lines)
                  end
                end
              }),
              prompt_title = "";
              results_title = "";
              prompt_prefix = "";
              min_length = 1;
              entry_prefix = "";
              selection_caret = "";
              border = true;
              finder = finders.new_table {
                results = keymaps_table,
                entry_maker = make_entry.gen_from_keymaps({}),
              },
              sorter = conf.generic_sorter(opts),
              attach_mappings = function(prompt_bufnr)
                actions.select_default:replace(function()
                  local selection = action_state.get_selected_entry()
                  if selection == nil then
                    utils.__warn_no_selection "builtin.keymaps"
                    return
                  end

                  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(selection.value.lhs, true, false, true), "t", true)
                  return actions.close(prompt_bufnr)
                end)
                return true
              end,
              }):find()
          end
       '';
     };
     searchCommands = {
      description = ''
        Search registered user commands
      '';
      vimCommandName = "SearchCommands";
      vimKeymapBinding = 
        {
          modes = ["n" "v" "i"];
          key = "<C-c>";
          silent = true;
        }
      ;
      action.__raw = ''
        function()
          local pickers = require('telescope.pickers')
          local finders = require('telescope.finders')
          local actions = require('telescope.actions')
          local action_state = require('telescope.actions.state')
          local make_entry = require "telescope.make_entry"
          local entry_display = require "telescope.pickers.entry_display"
          local sorters = require('telescope.sorters')
          local previewers = require('telescope.previewers')
          local conf = require("telescope.config").values

          local gen_from_commands = function()
            local displayer = entry_display.create {
              separator = "▏",
              items = {
                { remaining = true },
              }
            }
          local make_display = function(entry)
            local attrs = ""
            if entry.bang then
              attrs = attrs .. "!"
            end
            if entry.bar then
              attrs = attrs .. "|"
            end
            if entry.register then
              attrs = attrs .. '"'
            end
            return displayer {
              { entry.name, "TelescopeResultsIdentifier" },
            }
            end
            return function(entry)
              return make_entry.set_default_entry_mt({
                name = entry.name,
                bang = entry.bang,
                nargs = entry.nargs,
                complete = entry.complete,
                definition = entry.definition,
                --
                value = entry,
                desc = entry.desc,
                ordinal = entry.name,
                display = make_display,
              }, opts)
              end
            end
            pickers.new({
              layout_strategy = "cursor",
              layout_config = {
                height = 0.4,
                width = 0.6,
              },
              preview = {
                wrap = true,
              },
              previewer = previewers.new_buffer_previewer ({
                define_preview = function(self, entry, status)
                  local lines = {}
                  if entry.definition ~= nil then
                    for line in string.gmatch(entry.definition, "([^\n]*)") do
                      table.insert(lines, line)
                    end
                    vim.api.nvim_buf_set_lines(self.state.bufnr, 0, -1, false, lines)
                  end
                end
              }),
              prompt_title = "";
              results_title = "";
              prompt_prefix = "";
              min_length = 1;
              entry_prefix = "";
              selection_caret = "";
              border = true;
              finder = finders.new_table {
                results = (function()
                  local command_iter = vim.api.nvim_get_commands {}
                  local commands = {}
                  for _, cmd in pairs(command_iter) do
                    table.insert(commands, cmd)
                  end
                  local buf_command_iter = vim.api.nvim_buf_get_commands(0, {})
                  buf_command_iter[true] = nil -- remove the redundant entry
                  for _, cmd in pairs(buf_command_iter) do
                    table.insert(commands, cmd)
                  end
                  return commands
                end)(),
                entry_maker = gen_from_commands(),
              },
              sorter = conf.generic_sorter(opts),
              attach_mappings = function(prompt_bufnr)
                actions.select_default:replace(function()
                  local selection = action_state.get_selected_entry()
                  if selection == nil then
                    utils.__warn_no_selection "builtin.commands"
                    return
                  end
                  actions.close(prompt_bufnr)
                  local val = selection.value
                  local cmd = string.format([[:%s ]], val.name)
                  if val.nargs == "0" then
                    local cr = vim.api.nvim_replace_termcodes("<cr>", true, false, true)
                    cmd = cmd .. cr
                  end
                  vim.cmd [[stopinsert]]
                  vim.api.nvim_feedkeys(cmd, "nt", false)
                end)
                return true
              end,
              }):find()
          end
        '';
      };
      jumpDownHalfThePage = {
        description = "Jump down half of the screen height";
        vimCommandName = "JumpDownHalfAPage";
        vimKeymapBinding = 
          {
            modes = ["n" "v"];
            key = "<S-j>";
            silent = true;
          }
        ;
        # This emulates the default behavior or <C-d>
        action.__raw = ''
          function()
            -- Get the current window height
            local win_id = vim.api.nvim_get_current_win()
            local height = vim.api.nvim_win_get_height(win_id)

            -- Get the current cursor position (row, col)
            local cursor = vim.api.nvim_win_get_cursor(win_id)
            local current_row = cursor[1]

            -- Calculate the new row (half the window height below the current position)
            local new_row = current_row + math.floor(height / 2)

            -- Ensure the new row is within the bounds of the buffer
            local buf_line_count = vim.api.nvim_buf_line_count(0)
            new_row = math.min(new_row, buf_line_count)  -- Don't scroll past the last line

            -- Move the cursor to the new row (the column remains the same)
            vim.api.nvim_win_set_cursor(win_id, {new_row, cursor[2]})
          end
        '';
      };
      jumpUpHalfThePage = {
        description = "Jump up half of the screen height";
        vimCommandName = "JumpUpHalfAPage";
        vimKeymapBinding = 
          {
            modes = ["n" "v"];
            key = "<S-k>";
            silent = true;
          }
        ;
        # This emulates the default behavior or <C-u>
        action.__raw = ''
          function()
            -- Get the current window height
            local win_id = vim.api.nvim_get_current_win()
            local height = vim.api.nvim_win_get_height(win_id)

            -- Get the current cursor position (row, col)
            local cursor = vim.api.nvim_win_get_cursor(win_id)
            local current_row = cursor[1]

            -- Calculate the new row (half the window height below the current position)
            local new_row = current_row - math.floor(height / 2)

            -- Ensure the new row is within the bounds of the buffer
            local buf_line_count = vim.api.nvim_buf_line_count(0)
            new_row = math.max(new_row, 1)  -- Don't scroll above the first line

            -- Move the cursor to the new row (the column remains the same)
            vim.api.nvim_win_set_cursor(win_id, {new_row, cursor[2]})
          end
        '';
      };
      toggleNumberLine = {
        description = "Toggle line numbers";
        vimCommandName = "ToggleLineNumbers";
        vimKeymapBinding = {
          key = "<S-n>";
          modes = ["n"];
          silent = true;
        };
        action.__raw = ''
          function()
            vim.cmd('set relativenumber! nu!')
          end
        '';
      };
      checkDiagnostic = {
        description = ''
          Check the LSP diagnostic under the cursor
        '';
        vimCommandName = "CheckDiagnostic";
        vimKeymapBinding = {
          key = "DD";
          modes = ["n"];
          silent = true;
        };
        action.__raw = ''
          function()
            vim.diagnostic.open_float()
          end
        '';
      };
      focusSplitLeft = {
        description = ''
          Move focus to the split left of the current active split
        '';
        vimCommandName = "FocusSplitLeft";
        vimKeymapBinding = {
          key = "<C-h>";
          modes = ["n" "v" "i"];
          silent = true;
        };
        action.__raw = ''
          function()
            vim.cmd("wincmd h");
          end
        '';
      };
      focusSplitDown = {
        description = ''
          Move focus to the split below the current active split
        '';
        vimCommandName = "FocusSplitDown";
        vimKeymapBinding = {
          key = "<c-j>";
          modes = ["n" "v" "i"];
          silent = true;
        };
        action.__raw = ''
          function()
            vim.cmd("wincmd j");
          end
        '';
      };
      focusSplitUp = {
        description = ''
          Move focus to the split above the current active split
        '';
        vimCommandName = "FocusSplitUp";
        vimKeymapBinding = {
          key = "<C-k>";
          modes = ["n" "v" "i"];
          silent = true;
        };
        action.__raw = ''
          function()
            vim.cmd("wincmd k");
          end
        '';
      };
      focusSplitRight = {
        description = ''
          Move focus to the split to the right of the current active split
        '';
        vimCommandName = "FocusSplitRight";
        vimKeymapBinding = {
          key = "<C-l>";
          modes = ["n" "v" "i"];
          silent = true;
        };
        action.__raw = ''
          function()
            vim.cmd("wincmd l");
          end
        '';
      };
      jump = {
        description = ''
          Start a flash.nvim jump search
        '';
        vimCommandName = "Jump";
        vimKeymapBinding = {
          key = "s";
          modes = ["n" "v" "x"];
          silent = true;
        };
        action.__raw = ''
          function()
            require('flash').jump()
          end
        '';
      };
      copyUrlOfCurrentLineOnGithub = {
        description = ''
          Copy a URL of the line under the cursor on Github to the clipboard
        '';
        vimCommandName = "CopyUrlOfCurrentLineOnGithub";
        vimKeymapBinding = {
          key = "yg";
          # TODO: Implement this for a visual mode rand selection
          modes = ["n"];
          silent = true;
        };
        action.__raw = ''
          function()
            local gitlinker = require("gitlinker");
            local actions = require("gitlinker.actions");
            gitlinker.get_buf_range_url("n", {
              action_callback = actions.copy_to_clipboard
            });
          end
        '';
      };
      goToPreviousBuffer = {
        description = ''
          Navigate one buffer up in the buffer list
        '';
        vimCommandName = "GoToPreviousBuffer";
        vimKeymapBinding = {
          key = "<Left>";
          modes = ["n" "v" "i"];
          silent = true; 
        };
        action.__raw = ''
          function()
            vim.cmd("bprevious");
          end
        '';
      };
      goToNextBuffer = {
        description = ''
          Navigate one buffer down in the buffer list
        '';
        vimCommandName = "GoToNextBuffer";
        vimKeymapBinding = {
          key = "<Right>";
          modes = ["n" "v" "i"];
          silent = true; 
        };
        action.__raw = ''
          function()
            vim.cmd("bnext");
          end
        '';
      };
  };

  mkVimKeymap = key: command: {
    action.__raw = command.action.__raw;
    key = command.vimKeymapBinding.key;
    mode = command.vimKeymapBinding.modes;
    options = {
      desc = command.description;
      silent = command.vimKeymapBinding.silent;
    };
  };

  mkVimKeymaps = lib.attrValues (lib.mapAttrs mkVimKeymap commands);

  mkVimUserCommand = key: command: {
    ${command.vimCommandName} = {
      command.__raw = command.action.__raw;
      desc = command.description;
    };
  };
  mkVimUserCommands = lib.concatMapAttrs mkVimUserCommand commands;
in {
  programs.nixvim = {
    enable = true;
    vimAlias = true;
    opts.background = "dark";
    highlight.SignColumn.bg = "none";
    highlight.SignColumn.ctermbg = "none";
    colorschemes.gruvbox.enable = true;
    colorschemes.gruvbox.settings.transparent_mode = true;
    colorschemes.gruvbox.settings.overrides = {
      Comment = {
        bold = true;
      };
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
    highlightOverride.SatelliteBackground.link = "SignColumn";
    opts.undofile = true;
    opts.termguicolors = false;
    opts.autoindent = true;
    opts.smartindent = false;
    opts.smarttab = true;
    opts.tabstop = 8;
    opts.softtabstop = 0;
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
    opts.expandtab = true;
    opts.shiftwidth = 2;
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

    extraConfigVim = ''
      set laststatus=0
      hi! link StatusLine Normal
      hi! link StatusLineNC Normal
      set statusline=%{repeat('─',winwidth('.'))}
    '';

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
    ];
    plugins.telescope.enabledExtensions = ["live_grep_args"];
    plugins.markdown-preview.enable = true;
    plugins.markdown-preview.settings.auto_close = 0;
    plugins.markdown-preview.settings.auto_start = 0;
    plugins.markdown-preview.settings.combine_preview = 1;
    plugins.markdown-preview.settings.echo_preview_url = 1;
    plugins.markdown-preview.settings.refresh_slow = 1;
    plugins.markdown-preview.settings.page_title = "$${name}";
    plugins.markdown-preview.settings.markdown_css = "${pkgs.writeText "markdown-preview.css" ''
      /*
       * github like style
       * https://github.com/iamcco/markdown.css/blob/master/dest/github/markdown.css
       */

      :root {
          --color-text-primary: #333;
          --color-text-tertiary: #777;
          --color-text-link: #4078c0;
          --color-bg-primary: #fff;
          --color-bg-secondary: #fafbfc;
          --color-bg-tertiary: #f8f8f8;
          --color-border-primary: #ddd;
          --color-border-secondary: #eaecef;
          --color-border-tertiary: #d1d5da;
          --color-kbd-foreground: #444d56;
          --color-markdown-blockquote-border: #dfe2e5;
          --color-markdown-table-border: #dfe2e5;
          --color-markdown-table-tr-border: #c6cbd1;
          --color-markdown-code-bg: #1b1f230d;
      }
      [data-theme="dark"] {
          --color-text-primary: #c9d1d9;
          --color-text-tertiary: #8b949e;
          --color-text-link: #58a6ff;
          --color-bg-primary: #0d1117;
          --color-bg-secondary: #0d1117;
          --color-bg-tertiary: #161b22;
          --color-border-primary: #30363d;
          --color-border-secondary: #21262d;
          --color-border-tertiary: #6e7681;
          --color-kbd-foreground: #b1bac4;
          --color-markdown-blockquote-border: #3b434b;
          --color-markdown-table-border: #3b434b;
          --color-markdown-table-tr-border: #272c32;
          --color-markdown-code-bg: #f0f6fc26;
      }

      .markdown-body ol ol,
      .markdown-body ul ol,
      .markdown-body ol ul,
      .markdown-body ul ul,
      .markdown-body ol ul ol,
      .markdown-body ul ul ol,
      .markdown-body ol ul ul,
      .markdown-body ul ul ul {
        margin-top: 0;
        margin-bottom: 0;
      }
      .markdown-body {
        font-family: "Helvetica Neue", Helvetica, "Segoe UI", Arial, freesans, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
        font-size: 16px;
        color: var(--color-text-primary);
        line-height: 1.6;
        word-wrap: break-word;
        padding: 45px;
        background: var(--color-bg-primary);
        border: 1px solid var(--color-border-primary);
        -webkit-border-radius: 0 0 3px 3px;
        border-radius: 0 0 3px 3px;
      }
      .markdown-body > *:first-child {
        margin-top: 0 !important;
      }
      .markdown-body > *:last-child {
        margin-bottom: 0 !important;
      }
      .markdown-body .table-of-contents ol {
        list-style: none;
      }
      .markdown-body .table-of-contents > ol {
        padding-left: 0;
      }
      .markdown-body * {
        -webkit-box-sizing: border-box;
        -moz-box-sizing: border-box;
        box-sizing: border-box;
      }
      .markdown-body h1,
      .markdown-body h2,
      .markdown-body h3,
      .markdown-body h4,
      .markdown-body h5,
      .markdown-body h6 {
        margin-top: 1em;
        margin-bottom: 16px;
        font-weight: bold;
        line-height: 1.4;
      }
      .markdown-body h1 .anchor,
      .markdown-body h2 .anchor,
      .markdown-body h3 .anchor,
      .markdown-body h4 .anchor,
      .markdown-body h5 .anchor,
      .markdown-body h6 .anchor {
        margin-left: -24px;
        visibility: hidden;
      }
      .markdown-body h1:hover .anchor,
      .markdown-body h2:hover .anchor,
      .markdown-body h3:hover .anchor,
      .markdown-body h4:hover .anchor,
      .markdown-body h5:hover .anchor,
      .markdown-body h6:hover .anchor {
        visibility: visible;
      }
      .markdown-body p,
      .markdown-body blockquote,
      .markdown-body ul,
      .markdown-body ol,
      .markdown-body dl,
      .markdown-body table,
      .markdown-body pre {
        margin-top: 0;
        margin-bottom: 16px;
      }
      .markdown-body h1 {
        margin: 0.67em 0;
        padding-bottom: 0.3em;
        font-size: 2.25em;
        line-height: 1.2;
        border-bottom: 1px solid var(--color-border-secondary);
      }
      .markdown-body h2 {
        padding-bottom: 0.3em;
        font-size: 1.75em;
        line-height: 1.225;
        border-bottom: 1px solid var(--color-border-secondary);
      }
      .markdown-body h3 {
        font-size: 1.5em;
        line-height: 1.43;
      }
      .markdown-body h4 {
        font-size: 1.25em;
      }
      .markdown-body h5 {
        font-size: 1em;
      }
      .markdown-body h6 {
        font-size: 1em;
        color: var(--color-text-tertiary);
      }
      .markdown-body hr {
        margin-top: 20px;
        margin-bottom: 20px;
        height: 0;
        border: 0;
        border-top: 1px solid var(--color-border-primary);
      }
      .markdown-body ol,
      .markdown-body ul {
        padding-left: 2em;
      }
      .markdown-body ol ol,
      .markdown-body ul ol {
        list-style-type: lower-roman;
      }
      .markdown-body ol ul,
      .markdown-body ul ul {
        list-style-type: circle;
      }
      .markdown-body ol ul ul,
      .markdown-body ul ul ul {
        list-style-type: square;
      }
      .markdown-body ol {
        list-style-type: decimal;
      }
      .markdown-body ul {
        list-style-type: disc;
      }
      .markdown-body dl {
        margin-bottom: 1.3em
      }
      .markdown-body dl dt {
        font-weight: 700;
      }
      .markdown-body dl dd {
        margin-left: 0;
      }
      .markdown-body dl dd p {
        margin-bottom: 0.8em;
      }
      .markdown-body blockquote {
        margin-left: 0;
        margin-right: 0;
        padding: 0 15px;
        color: var(--color-text-tertiary);
        border-left: 4px solid var(--color-markdown-blockquote-border);
      }
      .markdown-body table {
        display: block;
        width: 100%;
        overflow: auto;
        word-break: normal;
        word-break: keep-all;
        border-collapse: collapse;
        border-spacing: 0;
      }
      .markdown-body table tr {
        background-color: var(--color-bg-primary);
        border-top: 1px solid var(--color-markdown-table-tr-border);
      }
      .markdown-body table tr:nth-child(2n) {
        background-color: var(--color-bg-tertiary);
      }
      .markdown-body table th,
      .markdown-body table td {
        padding: 6px 13px;
        border: 1px solid var(--color-markdown-table-border);
        vertical-align: top;
      }
      .markdown-body kbd {
        display: inline-block;
        padding: 5px 6px;
        font: 14px SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace;
        line-height: 10px;
        color: var(--color-kbd-foreground);
        vertical-align: middle;
        background-color: var(--color-bg-secondary);
        border: 1px solid var(--color-border-tertiary);
        border-radius: 3px;
        box-shadow: inset 0 -1px 0 var(--color-border-tertiary);
      }
      .markdown-body pre {
        word-wrap: normal;
        padding: 16px;
        overflow: auto;
        font-size: 85%;
        line-height: 1.45;
        background-color: var(--color-bg-tertiary);
        -webkit-border-radius: 3px;
        border-radius: 3px;
      }
      .markdown-body pre code {
        display: inline;
        max-width: initial;
        padding: 0;
        margin: 0;
        overflow: initial;
        font-size: 100%;
        line-height: inherit;
        word-wrap: normal;
        white-space: pre;
        border: 0;
        -webkit-border-radius: 3px;
        border-radius: 3px;
        background-color: transparent;
      }
      .markdown-body pre code:before,
      .markdown-body pre code:after {
        content: normal;
      }
      .markdown-body code {
        font-family: Consolas, "Liberation Mono", Menlo, Courier, monospace;
        padding: 0;
        padding-top: 0.2em;
        padding-bottom: 0.2em;
        margin: 0;
        font-size: 85%;
        background-color: var(--color-markdown-code-bg);
        -webkit-border-radius: 3px;
        border-radius: 3px;
      }
      .markdown-body code:before,
      .markdown-body code:after {
        letter-spacing: -0.2em;
        content: "\00a0";
      }
      .markdown-body a {
        color: var(--color-text-link);
        text-decoration: none;
        background: transparent;
      }
      .markdown-body img {
        max-width: 100%;
        max-height: 100%;
      }
      .markdown-body strong {
        font-weight: bold;
      }
      .markdown-body em {
        font-style: italic;
      }
      .markdown-body del {
        text-decoration: line-through;
      }
      .task-list-item {
        list-style-type: none;
      }
      .task-list-item input {
        font: 13px/1.4 Helvetica, arial, nimbussansl, liberationsans, freesans, clean, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
        margin: 0 0.35em 0.25em -1.6em;
        vertical-align: middle;
      }
      .task-list-item input[disabled] {
        cursor: default;
      }
      .task-list-item input[type="checkbox"] {
        -webkit-box-sizing: border-box;
        -moz-box-sizing: border-box;
        box-sizing: border-box;
        padding: 0;
      }
      .task-list-item input[type="radio"] {
        -webkit-box-sizing: border-box;
        -moz-box-sizing: border-box;
        box-sizing: border-box;
        padding: 0;
      }

      #page-ctn {
      	  max-width: 100%;
      }

             .mermaid {
        overflow: scroll;
             }
    ''}";

    extraConfigLua = ''
       require('satellite').setup({
      current_only = false,
       winblend = 0,
       zindex = 40,
       excluded_filetypes = {},
       width = 2,
       handlers = {
         cursor = {
           enable = true,
           -- Supports any number of symbols
           symbols = { '⎺', '⎻', '⎼', '⎽' }
           -- symbols = { '⎻', '⎼' }
           -- Highlights:
           -- - SatelliteCursor (default links to NonText
         },
         search = {
           enable = true,
           -- Highlights:
           -- - SatelliteSearch (default links to Search)
           -- - SatelliteSearchCurrent (default links to SearchCurrent)
         },
         diagnostic = {
           enable = true,
           signs = {'-', '=', '≡'},
           min_severity = vim.diagnostic.severity.HINT,
           -- Highlights:
           -- - SatelliteDiagnosticError (default links to DiagnosticError)
           -- - SatelliteDiagnosticWarn (default links to DiagnosticWarn)
           -- - SatelliteDiagnosticInfo (default links to DiagnosticInfo)
           -- - SatelliteDiagnosticHint (default links to DiagnosticHint)
         },
         gitsigns = {
           enable = true,
           signs = { -- can only be a single character (multibyte is okay)
      add = "│",
      change = "│",
      delete = "-",
           },
           -- Highlights:
           -- SatelliteGitSignsAdd (default links to GitSignsAdd)
           -- SatelliteGitSignsChange (default links to GitSignsChange)
           -- SatelliteGitSignsDelete (default links to GitSignsDelete)
         },
         marks = {
           enable = true,
           show_builtins = false, -- shows the builtin marks like [ ] < >
           key = 'm'
           -- Highlights:
           -- SatelliteMark (default links to Normal)
         },
         quickfix = {
           signs = { '-', '=', '≡' },
           -- Highlights:
           -- SatelliteQuickfix (default links to WarningMsg)
         }
       },
       });
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
              vim.keymap.set({'n', 'v'}, 'j', 'gj', {buffer = true})
              vim.keymap.set({'n', 'v'}, 'k', 'gk', {buffer = true})
            end
          '';
        };
      }
      {
        event = ["BufRead"];
        # TODO: Fine a real syntax highlighting solution for haxe
        pattern = ["*.hx"];
        command = "set filetype=ts";
      }
      {
        event = ["TextYankPost"];
        pattern = ["*"];
        command = ''lua vim.highlight.on_yank({higroup="ActiveYank", timeout=300})'';
      }
      {
        event = ["User"];
        pattern = ["TelescopePreviewerLoaded"];
        command = ''lua vim.wo.wrap = true'';
      }
    ];
    keymaps = [
      # Custom keymaps can be added here if needed, but I stick to using
      # `mkVimKeymaps` and the `commands` data structure it references.
    ] ++ mkVimKeymaps;

    userCommands = {
      # Custom commands can be added here if needed, but I stick to using
      # `mkVimUserCommand` and the `commands` data structure it references.
    } // mkVimUserCommands;
  };
}
