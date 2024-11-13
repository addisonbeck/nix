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
