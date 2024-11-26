{conf, ...}: {
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
    # {
    #   event = [
    #     "BufEnter"
    #     "FileChangedShellPost"
    #     "Syntax"
    #     "TextChanged"
    #     "TextChangedI"
    #     "InsertLeave"
    #     "WinScrolled"
    #   ];
    #   pattern = [
    #     "*.markdown"
    #     "*.md"
    #   ];
    #   callback = {
    #     __raw = ''
    #       function()
    #         local function get_code_blocks()
    #           local parser = vim.treesitter.get_parser(0)
    #           local tree = parser:parse()[1]
    #           local root = tree:root()
    #
    #           local function find_code_blocks(node)
    #             local result = {}
    #             local function traverse(node)
    #               if node:type() == 'fenced_code_block' then
    #                 local start_row, start_col, end_row, end_col = node:range()
    #                 table.insert(result, {start_row, start_col, end_row, end_col})
    #               end
    #               for child in node:iter_children() do
    #                 traverse(child)
    #               end
    #             end
    #             traverse(node)
    #             return result
    #           end
    #
    #           return find_code_blocks(root)
    #         end
    #
    #         local code_blocks = get_code_blocks()
    #         local namespace = vim.api.nvim_create_namespace('ab_md_code_blocks')
    #         vim.api.nvim_buf_clear_namespace(0, namespace, 0, -1)
    #         for _, node in ipairs(code_blocks) do
    #           local start_row, start_col, end_row, end_col = unpack(node)
    #           vim.api.nvim_buf_set_extmark(0, namespace, start_row, start_col, {
    #             end_row = end_row,
    #             end_col = end_col,
    #             hl_group = "CodeFence",
    #             hl_eol = true,
    #           })
    #         end
    #       end
    #     '';
    #   };
    # }
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
