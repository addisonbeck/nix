function()
  local utils = require('telescope.utils');
  local conf = require("telescope.config").values

  local marks = {}
  local current_buf = vim.api.nvim_get_current_buf();
  local bufname = vim.api.nvim_buf_get_name(current_buf);
  local local_marks = {
    items = vim.fn.getmarklist(current_buf),
    name_func = function(_, line)
      return vim.api.nvim_buf_get_lines(current_buf, line - 1, line, false)[1]
    end,
  }
  local global_marks = {
    items = vim.fn.getmarklist(),
    name_func = function(mark, _)
      -- get buffer name if it is opened, otherwise get file name
      return vim.api.nvim_get_mark(mark, {})[4]
    end,
  }

  local marks_table = {}
  local marks_others = {}
  local all_marks = { local_marks, global_marks }
  -- for _, mark in ipairs(all_marks) do
  --   local mark_name = mark.name
  --   local line_number = mark.line
  --   local file_name = mark.file
  --   local line_text = vim.fn.getline(line_number)
  --   table.insert(marks, {
  --     value = { file = file_name, line = line_number, text = line_text, mark = mark_name },
  --     --display = string.format("%s: %s (%d): %s", mark_name, file_name, line_number, line_text),
  --     display = mark_name .. ": " .. line_text,
  --     ordinal = mark_name .. ": " .. file_name .. " " .. line_text,
  --   })
  -- end
  for _, cnf in ipairs(all_marks) do
    for _, v in ipairs(cnf.items) do
      -- strip the first single quote character
      local mark = string.sub(v.mark, 2, 3)
      local _, lnum, col, _ = unpack(v.pos)
      local name = cnf.name_func(mark, lnum)
      -- same format to :marks command
      local line = string.format("%s %6d %4d %s", mark, lnum, col - 1, name);
      local filename = utils.path_expand(v.file or bufname);
      local file_lines = vim.fn.readfile(filename);
      local line_text = "Line text not found!";
      if file_lines[lnum] then
        line_text = file_lines[lnum]:gsub("^%s+", "");
      end
      local row = {
        -- line = line,
        -- lnum = lnum,
        -- col = col,
        -- filename = utils.path_expand(v.file or bufname),
        value = {
          filename = filename,
          line = lnum,
          col = col,
        },
        display = mark .. ": " .. line_text,
        ordinal = line_text .. " " .. mark .. " " .. filename,
      }
      -- I don't care about non-letter marks
      if mark:match "%a" then
        table.insert(marks_table, row)
      end
      --   table.insert(marks_others, row)
      -- end
    end
  end
  -- marks_table = vim.fn.extend(marks_table, marks_others)
  require('telescope.pickers').new({}, {
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
    finder = require('telescope.finders').new_table({
      results = marks_table,
      entry_maker = function(entry)
        return {
          value = entry.value,
          display = entry.display,
          ordinal = entry.ordinal,
        }
      end
    }),
    sorter = require('telescope.sorters').get_fuzzy_file(),
    --previewer = (require("telescope.previewers")).vim_buffer_vimgrep.new(""),
    attach_mappings = function(prompt_bufnr, map)
      map('i', '<CR>', function()
        local selection = require('telescope.actions.state').get_selected_entry()
        require('telescope.actions').close(prompt_bufnr)
        vim.cmd('e! ' .. selection.value.filename)
        vim.fn.cursor(selection.value.line, selection.value.col)
      end)
      return true
  end
  }):find()
end
