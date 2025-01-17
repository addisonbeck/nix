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
          require('telescope.builtin').git_files({
            prompt_title = "",
            results_title = "",
            border = true,
            borderchars = {
              prompt = { "─", "│", " ", "│", "╭", "╮", "│", "│" },
              results = { "─", "│", "─", "│", "├", "┤", "╯", "╰" },
              preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
            },
            layout_strategy = "center",
            layout_config = {
              height = 0.4,
              width = 0.6,
            },
          })
        end
      '';
    };
    searchDefinitions = {
      description = ''
        Search lsp definitions of the symbol under the cursor
      '';
      vimCommandName = "SearchDefintions";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<Space>d";
        silent = true;
      };
      action.__raw = ''
        function()
          require('telescope.builtin').lsp_definitions()
        end
      '';
    };
    searchImplementations = {
      description = ''
        Search lsp implementations of the symbol under the cursor
      '';
      vimCommandName = "SearchImplementations";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<Space>i";
        silent = true;
      };
      action.__raw = ''
        function()
          require('telescope.builtin').lsp_implementations()
        end
      '';
    };
    searchReferences = {
      description = ''
        Search lsp references of the symbol under the cursor
      '';
      vimCommandName = "SearchReferences";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<Space>r";
        silent = true;
      };
      action.__raw = ''
        function()
          require('telescope.builtin').lsp_references()
        end
      '';
    };
    searchMarks = {
      description = ''
        Search for marks with telescope.
      '';
      vimCommandName = "SearchMarks";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<C-m>";
        silent = true;
      };
      action.__raw = builtins.readFile ./search_marks.lua;
    };
    searchNotes = {
      description = ''Grep search my notes'';
      vimCommandName = "SearchNotes";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<C-n>";
        silent = true;
      };
      action.__raw = ''
        function()
          require('telescope.builtin').git_files({
            cwd = '~/notes',
            color_devicons = false,
            prompt_title = "",
            results_title = "",
            border = true,
            borderchars = {
              prompt = { "─", "│", " ", "│", "╭", "╮", "│", "│" },
              results = { "─", "│", "─", "│", "├", "┤", "╯", "╰" },
              preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
            },
            layout_strategy = "center",
            layout_config = {
              --anchor = "N",
              height = 0.4,
              width = 0.6,
            },
          })
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
        key = "<C-0>";
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
        key = "<Space>.";
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
        modes = ["n" "v"];
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
        modes = ["n" "v"];
        key = "<Space>R";
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
        modes = ["n" "v"];
        # I'd rather this be mapped to save all quit
        key = "<C-z>";
        silent = true;
      };
      action.__raw = ''
        function()
          require('telescope.builtin').spell_suggest({
            borderchars = {
              prompt = { "─", "│", " ", "│", "╭", "╮", "│", "│" },
              results = { "─", "│", "─", "│", "├", "┤", "╯", "╰" },
            },
            prompt_title = "";
            results_title = "";
            prompt_prefix = "";
            entry_prefix = "";
            selection_caret = "";
            layout_strategy = "cursor",
            layout_config = {
              height = 0.4,
              width = 0.2,
            },
          })
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
      action.__raw = builtins.readFile ./search_buffers.lua;
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
        modes = ["n" "v"];
        key = "<Space>crp";
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
        modes = ["n" "v"];
        key = "<Space>cfp";
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
        modes = ["n" "v"];
        key = "<Space>cfn";
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
        modes = ["n" "v"];
        key = "<Space>k";
        silent = true;
      };
      action.__raw = builtins.readFile ./search_keymaps.lua;
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
      action.__raw = builtins.readFile ./search_commands.lua;
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
      action.__raw = builtins.readFile ./jump_down_half_a_page.lua;
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
      action.__raw = builtins.readFile ./jump_up_half_a_page.lua;
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
    checkHover = {
      description = ''
        Check the LSP hover under the cursor
      '';
      vimCommandName = "Hover";
      vimKeymapBinding = {
        key = "HH";
        modes = ["n"];
        silent = true;
      };
      action.__raw = ''
        function()
          vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
            vim.lsp.handlers.hover,
            {
              -- ["single", "double", "rounded", "solid", "shadow", or a list
              -- of characters]
              border = "solid",
              max_width = 80,
              max_height = 20,
            }
          )
          vim.lsp.buf.hover()
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
        key = "<C-j>";
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
    superSearch = {
      description = ''
        Search buffers, oldfiles, git_files, and notes.
      '';
      vimCommandName = "SuperSearch";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<C-s>";
        silent = true;
      };
      action.__raw = builtins.readFile ./super_search.lua;
    };
    mkNote = {
      description = ''
        Create and open a new file in my notes
      '';
      vimCommandName = "MkNote";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "MKN";
        silent = true;
      };
      action.__raw = ''
        function()
          vim.ui.input({ prompt = 'Note name: ' }, function(input)
            if input then
              local home_dir = os.getenv("HOME")
              local file, err = io.open(home_dir .. "/notes/" .. input .. ".md", "w")
              vim.cmd("e ~/notes/" .. input .. ".md")
            end
          end)
        end
      '';
    };
    zenMode = {
      description = ''
        Toggle zen mode
      '';
      vimCommandName = "ZM";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<Space>z";
        silent = true;
      };
      action.__raw = ''
        function()
          require("zen-mode").toggle()
        end
      '';
    };
    redo = {
      description = ''
        Redo the last undo (replaces <Ctrl-r>)
      '';
      vimCommandName = "Redo";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "U";
        silent = true;
      };
      action.__raw = ''
        function()
          vim.api.nvim_command('redo')
        end
      '';
    };
    codeCompanion = {
      description = ''
        Toggle CodeCompanion
      '';
      vimCommandName = "ToggleCodeCompanion";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<C-a>";
        silent = true;
      };
      action.__raw = ''
        function()
          vim.cmd("CodeCompanionChat Toggle");
        end
      '';
    };
    talkToANorwegianBoatCaptain = {
      description = ''
        Speak with a norweigian boat captain who codes.
      '';
      vimCommandName = "TalkToCapn";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<Space>aic";
        silent = true;
      };
      action.__raw = ''
        function()
          local target_name = "[CodeCompanion]: Captain"
          for _, buf in ipairs(vim.api.nvim_list_bufs()) do
            local name = vim.api.nvim_buf_get_name(buf)
            if name:match(target_name .. "$") then
              return
            end
          end
          require("codecompanion").prompt("captain")
          local bufnr = vim.api.nvim_get_current_buf()
          vim.api.nvim_buf_set_name(bufnr, target_name)
        end
      '';
    };
    talkToASecurityAuditor = {
      description = ''
        Speak with a security auditor
      '';
      vimCommandName = "TalkToAuditor";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<Space>aia";
        silent = true;
      };
      action.__raw = ''
        function()
          local target_name = "[CodeCompanion]: Security Auditor"
          for _, buf in ipairs(vim.api.nvim_list_bufs()) do
            local name = vim.api.nvim_buf_get_name(buf)
            if name:match(target_name .. "$") then
              return
            end
          end
          require("codecompanion").prompt("auditor")
          local bufnr = vim.api.nvim_get_current_buf()
          vim.api.nvim_buf_set_name(bufnr, target_name)
        end
      '';
    };
    githubPrUnderCursor = {
      description = ''
        Open the PR at the URL under the cursor using Octo
      '';
      vimCommandName = "OctoUnderCursor";
      vimKeymapBinding = {
        modes = ["n"];
        key = "<Space>oe";
        silent = true;
      };
      action.__raw = ''
        function()
            local url = vim.fn.expand('<cWORD>')
            url = url:gsub('^[%p]*(.-)[%p]*$', '%1')

            -- Check if it's a valid GitHub Issue or PR URL
            local pr_pattern = "github.com/([%w-]+)/([%w-]+)/pull/(%d+)"
            local issue_pattern = "github.com/([%w-]+)/([%w-]+)/issues/(%d+)"

            local owner, repo, number = url:match(pr_pattern)
            local is_pr = true

            if not owner then
                owner, repo, number = url:match(issue_pattern)
                is_pr = false
            end

            if not owner then
                vim.notify("Not a valid GitHub Issue or PR URL", vim.log.levels.ERROR)
                return
            end

            -- Just pass the components separately
            local cmd = string.format('Octo %s edit %s/%s %s',
                is_pr and "pr" or "issue",
                owner,
                repo,
                number)

            local success, error = pcall(vim.cmd, cmd)
            if not success then
                vim.notify("Failed to open: " .. tostring(error), vim.log.levels.ERROR)
            end
        end
      '';
    };
    # This doesn't work
    githubFetchSummaryPrUnderCursor = {
      description = ''
        Gets a summary of the PR under the cursor and adds it to the
        clipboard
      '';
      vimCommandName = "OctoUnderCursor";
      vimKeymapBinding = {
        modes = ["n"];
        key = "<Space>os";
        silent = true;
      };
      action.__raw = ''
        function()
            local url = vim.fn.expand('<cWORD>')
            url = url:gsub('^[%p]*(.-)[%p]*$', '%1')
            local pr_pattern = "github.com/([%w-]+)/([%w-]+)/pull/(%d+)"
            local owner, repo, number = url:match(pr_pattern)
            if not owner then
                vim.notify("Not a valid GitHub PR URL", vim.log.levels.ERROR)
                return
            end

            -- Use gh cli to fetch PR data in JSON format from the current directory
            local cmd = string.format('gh pr view %s --repo %s/%s --json title,body', number, owner, repo)
            local handle = io.popen(cmd)
            local result = handle:read("*a")
            handle:close()

            local ok, data = pcall(vim.fn.json_decode, result)
            if not ok then
                vim.notify("Failed to parse GitHub PR data", vim.log.levels.ERROR)
                return
            end

            -- Extract tracking information from body
            local body = data.body or ""
            local tracking_id = body:match('[Tt]racking.-#([A-Z0-9%-]+)') or "Not found"
            local tracking_url = body:match('[Tt]racking.-%(([^%)]+)%)') or "Not found"

            -- Get current cursor position
            local row = vim.api.nvim_win_get_cursor(0)[1] - 1

            local text = string.format([[# %s
        * Jira ID: %s
        * Jira URL: %s
        * PR: %s]],
                data.title or "No title found",
                tracking_id,
                tracking_url,
                url)

            -- Split and insert the text
            local lines = vim.split(text, "\n", { plain = true })
            vim.api.nvim_buf_set_text(0, row, 0, row, 0, lines)
        end
      '';
    };
    tmuxSessionSwitcher = {
      description = "Tmux: Open window switcher";
      vimCommandName = "SearchTmuxWindows";
      vimKeymapBinding = {
        key = "<C-j>";
        modes = ["n"];
        silent = true;
      };
      action.__raw = ''
        function()
          vim.fn.jobstart('tmux display-popup -E "fzf-tmux-popup"', {
            detach = true
          })
          vim.cmd('redraw!')
        end
      '';
    };
    tmuxPopup = {
      description = "Toggle a persistent tmux popup for the current Neovim instance";
      vimCommandName = "ToggleTmuxPopup";
      vimKeymapBinding = {
        modes = ["n" "v"];
        key = "<C-p>";
        silent = true;
      };
      action.__raw = ''
        function()
          -- Get Neovim's server name as unique identifier
          local nvim_id = vim.v.servername:gsub("/", "_")
          local popup_session = "nvim_popup_" .. nvim_id

          -- Check if the popup exists
          local check_cmd = string.format("tmux has-session -t %s 2>/dev/null", popup_session)
          local popup_exists = os.execute(check_cmd)

          if popup_exists then
            -- If popup exists, toggle it off
            vim.fn.system(string.format("tmux kill-session -t %s", popup_session))
          else
            -- Create new persistent session and show popup
            local cmd = string.format([[
              tmux new-session -d -s %s;
              tmux display-popup -E \
                -w 80%% -h 80%% \
                -x 10%% -y 10%% \
                "tmux attach -t %s"
            ]], popup_session, popup_session)

            vim.fn.jobstart(cmd, {
              detach = true,
              on_exit = function()
                vim.cmd('redraw!')
              end
            })
          end
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
