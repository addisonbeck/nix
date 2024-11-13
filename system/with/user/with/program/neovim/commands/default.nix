{lib, ...}: let
  mkVimKeymap = key: command: {
    action.__raw = command.action.__raw;
    key = command.vimKeymapBinding.key;
    mode = command.vimKeymapBinding.modes;
    options = {
      desc = command.description;
      silent = command.vimKeymapBinding.silent;
    };
  };

  mkVimUserCommand = key: command: {
    ${command.vimCommandName} = {
      command.__raw = command.action.__raw;
      desc = command.description;
    };
  };

  mkVimKeymaps = commandList: lib.attrValues (lib.mapAttrs mkVimKeymap commandList);
  mkVimUserCommands = commandList: lib.concatMapAttrs mkVimUserCommand commandList;

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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-g>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-d>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-a>";
        silent = true;
      };
      action.__raw = ''
        function()
          require('telescope.builtin').marks();
        end
      '';
    };
    searchNotes = {
      description = ''Grep search my notes'';
      vimCommandName = "SearchNotes";
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-n>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-l>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-5>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-\>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-.>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        # I'd rather this be mapped to save all quit
        key = "<C-q>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n"];
        key = "CA";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-b>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-o>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-d>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-1>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-2>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-f>";
        silent = true;
      };
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
      '';
      vimCommandName = "CopyRelativePath";
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-p>r";
        silent = true;
      };
      action.__raw = ''
        function()
          require("where-am-i.commands").copy_file_name({
            content = {
              file_path = {
                format = "present_working_dir_path"
              }
            }
          });
        end
      '';
    };
    copyFullPath = {
      description = ''
        Copy the full system path of the file loaded in the open buffer
      '';
      vimCommandName = "CopyFullPath";
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-p>f";
        silent = true;
      };
      action.__raw = ''
        function()
          require("where-am-i.commands").copy_file_name({
            content = {
              file_path = {
                format = "system_path"
              }
            }
          });
        end
      '';
    };
    copyFileName = {
      description = ''
        Copy the file name of the file loaded in the open buffer
      '';
      vimCommandName = "CopyFileName";
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-p>n";
        silent = true;
      };
      action.__raw = ''
        function()
          require("where-am-i.commands").copy_file_name({
            content = {
              file_path = {
                format = "filename_only"
              }
            }
          });
        end
      '';
    };
    generateGuid = {
      description = ''
        Generate a guid and paste it under the cursor
      '';
      vimCommandName = "GenerateGuid";
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-g>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-->";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-0>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-k>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v" "i"];
        key = "<C-c>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<S-j>";
        silent = true;
      };
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
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<S-k>";
        silent = true;
      };
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
  };
in {
  keymaps =
    [
      # Custom keymaps can be added here if needed, but I stick to using
      # `mkVimKeymaps` and the `commands` data structure it references.
    ]
    ++ mkVimKeymaps commands;

  userCommands =
    {
      # Custom commands can be added here if needed, but I stick to using
      # `mkVimUserCommand` and the `commands` data structure it references.
    }
    // mkVimUserCommands commands;
}
