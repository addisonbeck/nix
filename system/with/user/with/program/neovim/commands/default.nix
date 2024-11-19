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
        modes = ["n" "v"];
        key = "<Space>r";
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
        modes = ["n" "v" ];
        key = "<Space><Space>";
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
      action.__raw = (builtins.readFile ./search_buffers.lua);
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
        key = "<C-t>";
        silent = true;
      };
      action.__raw = ''
        function()
          require('telescope').extensions.file_browser.file_browser({
            depth = 1,
            display_stat = false,
            git_status = false,
            layout_strategy = "horizontal",
            prompt_title = "";
            --results_title = "";
            preview_title = "";
            prompt_prefix = "";
            layout_config = {
              height = 0.99,
              preview_cutoff = 0,
              prompt_position = "bottom",
              width = 0.99,
              preview_width = 0.6,
            },
          });
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
      action.__raw = (builtins.readFile ./search_keymaps.lua);
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
      action.__raw = (builtins.readFile ./search_commands.lua);
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
      action.__raw = (builtins.readFile ./jump_down_half_a_page.lua);
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
      action.__raw = (builtins.readFile ./jump_up_half_a_page.lua);
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
