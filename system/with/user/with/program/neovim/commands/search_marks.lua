function(opts)
  local bufnr = vim.api.nvim_buf_get_name(0)
  local local_marks = {
    items = vim.fn.getmarklist(bufnr),
    name_func = function(_, line)
      return vim.api.nvim_buf_get_lines(bufnr, line - 1, line, false)[1]
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
  local bufname = vim.api.nvim_buf_get_name(opts.bufnr)
  local all_marks = {}
  opts.mark_type = vim.F.if_nil(opts.mark_type, "all")
  if opts.mark_type == "all" then
    all_marks = { local_marks, global_marks }
  elseif opts.mark_type == "local" then
    all_marks = { local_marks }
  elseif opts.mark_type == "global" then
    all_marks = { global_marks }
  end

  for _, cnf in ipairs(all_marks) do
    for _, v in ipairs(cnf.items) do
      -- strip the first single quote character
      local mark = string.sub(v.mark, 2, 3)
      local _, lnum, col, _ = unpack(v.pos)
      local name = cnf.name_func(mark, lnum)
      -- same format to :marks command
      local line = string.format("%s %6d %4d %s", mark, lnum, col - 1, name)
      local row = {
        line = line,
        lnum = lnum,
        col = col,
        filename = utils.path_expand(v.file or bufname),
      }
      -- non alphanumeric marks goes to last
      if mark:match "%w" then
        table.insert(marks_table, row)
      else
        table.insert(marks_others, row)
      end
    end
  end
  marks_table = vim.fn.extend(marks_table, marks_others)

  pickers
    .new(opts, {
      prompt_title = "Marks",
      finder = finders.new_table {
        results = marks_table,
        entry_maker = opts.entry_maker or make_entry.gen_from_marks(opts),
      },
      previewer = conf.grep_previewer(opts),
      sorter = conf.generic_sorter(opts),
      push_cursor_on_edit = true,
      push_tagstack_on_edit = true,
    })
    :find()
end
