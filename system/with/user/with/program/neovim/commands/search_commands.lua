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
      border = true;
      borderchars = {
        prompt = { "─", "│", " ", "│", "╭", "╮", "│", "│" },
        results = { "─", "│", "─", "│", "├", "┤", "╯", "╰" },
        preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
      },
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
