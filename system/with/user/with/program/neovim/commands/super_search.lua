-- TODO: This does not work at all
function()
  local sorters = require('telescope.sorters');
  local builtin = require('telescope.builtin');
  local finders = require "telescope.finders";
  local pickers = require "telescope.pickers";
  local conf = require("telescope.config").values;

  local opts = conf.default or {}
  opts.prompt_title = "Combined Files";
  opts.sorter = sorters.get_fuzzy_file();
  opts.finder = finders.new_table({
    results = vim.tbl_extend("force",
      (builtin.buffers({}) or {}),
      (builtin.oldfiles({}) or {}),
      (builtin.git_files({}) or {})
    );
  });
  pickers.new(opts):find();
end
