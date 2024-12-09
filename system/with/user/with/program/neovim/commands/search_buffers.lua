function()
  local actions = require('telescope.actions')
  local actions_state = require('telescope.actions.state')
  local pickers = require('telescope.pickers')
  local finders = require('telescope.finders')
  local sorters = require('telescope.sorters')
  local themes = require('telescope.themes')
  local utils = require('telescope.utils')
  local conf = require("telescope.config").values

  local function get_buffers()
    local buffers = {}
    for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_loaded(bufnr) then
        local filename = vim.api.nvim_buf_get_name(bufnr)
        if filename ~= "" then
          table.insert(buffers, { filename, bufnr })
        end
      end
    end
    return buffers
  end

  local buffers = get_buffers()

  pickers.new({
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
      width = 0.4,
    },
    finder = finders.new_table {
      results = buffers,
      entry_maker = function(entry)
        local file_name = vim.fn.fnamemodify(entry[1], ":t")
        return {
          value = entry[2],
          display = vim.fn.fnamemodify(entry[1], ":h:t") .. "/" .. file_name,
          ordinal = entry[1],
          filename = entry[1]
        }
      end,
    },
    sorter = sorters.get_generic_fuzzy_sorter(),
    --previewer = conf.grep_previewer({}),

  attach_mappings = function(prompt_bufnr, map)
    map('i', '<CR>', function()
      local selection = actions_state.get_selected_entry()
      actions.close(prompt_bufnr)
      vim.api.nvim_set_current_buf(selection.value)
    end)
    map('n', 'd', function()
      local selection = actions_state.get_selected_entry()
      actions.close(prompt_bufnr)
      vim.api.nvim_buf_delete(selection.value, { force = true })
    end)
    return true
  end,
  }):find()
end
