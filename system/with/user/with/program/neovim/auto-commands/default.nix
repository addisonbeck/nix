{...}: {
  autoCommands = [
    # disable semantic highlighting
    {
      event = ["LspAttach"];
      callback = {
        __raw = ''
          function(args)
            local client = vim.lsp.get_client_by_id(args.data.client_id)
            client.server_capabilities.semanticTokensProvider = nil
          end
        '';
      };
    }
    {
      event = [
        "FileType"
      ];
      pattern = [
        "qf"
        "help"
        "man"
        "lspinfo"
      ];
      callback = {
        __raw = ''
          function()
          vim.cmd([[
              set buflisted
              wincmd o
          ]])
          end
        '';
      };
    }
    {
      event = [
        "BufEnter"
      ];
      pattern = [
        "*.markdown"
        "*.md"
      ];
      callback = {
        __raw = ''
          function()
            local ts_utils = require 'nvim-treesitter.ts_utils'
            local buf = vim.api.nvim_get_current_buf()

            -- Function to get all function declarations in the current buffer
            local function get_function_declarations()
              -- Get the current buffer

              -- Get the root node of the current buffer's syntax tree
              local parser = vim.treesitter.get_parser(buf)
              local tree = parser:parse()[1]  -- The first tree (in case there are multiple trees for different languages)

              -- Get the root node of the syntax tree
              local root = tree:root()

              -- Function to recursively traverse the tree and find function declarations
              local function find_functions(node)
                local result = {}
                
                -- Check if the current node is a function declaration (for example, "function_declaration")
                if node:type() == '@markup.raw.block.markdown' then
                  -- Get the start and end positions of the function declaration
                  local start_row, start_col, end_row, end_col = node:range()
                  table.insert(result, {start_row, start_col, end_row, end_col})
                end

                return result
              end

              -- Start from the root node and search for function declarations
              local nodes = find_functions(root)


              for node in nodes do
              end
            end
            -- Example usage: Get all function declarations and print their positions
            local functions = get_function_declarations()
            local extmark_opts = {
              virt_text = {{"<-- Symbol -->", "Comment"}},  -- You can customize this
              virt_text_pos = 'eol',  -- Position at the end of the line
            }
            for _, func in ipairs(functions) do
              vim.api.nvim_buf_set_extmark(buf, 0, start_row, start_col, extmark_opts)
            end
          end
        '';
      };
    }
    {
      event = ["BufWinEnter"];
      # TODO: Fine a real syntax highlighting solution for haxe
      pattern = ["*.hx"];
      command = "set filetype=ts";
    }
    {
      event = ["TextYankPost"];
      pattern = ["*"];
      command = ''lua vim.highlight.on_yank({higroup="ActiveYank", timeout=300})'';
    }
    {
      event = ["User"];
      pattern = ["TelescopePreviewerLoaded"];
      command = ''lua vim.wo.wrap = true'';
    }
  ];
}
