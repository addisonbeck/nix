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
