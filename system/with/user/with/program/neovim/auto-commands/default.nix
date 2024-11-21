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
        "*.txt"
        "gitcommit"
        "markdown"
        "md"
      ];
      callback = {
        __raw = ''
          function()
            local zm = require("zen-mode")
            local win = vim.api.nvim_get_current_win()
            local buffer_id = vim.api.nvim_win_get_buf(win)
            -- vim.api.nvim_set_current_buf(buffer_id)
            -- -- pcall(zm.close);
            -- pcall(zm.toggle, {
            --   window = { 
            --     width = 85,
            --     --height = 1,
            --   }
            -- })
            pcall(zm.close, {
              window = { 
                width = 100,
                --height = 1,
              }
            })
            vim.api.nvim_set_current_buf(buffer_id)
            pcall(zm.open, {
              window = { 
                width = 85,
                --height = 1,
              }
            })
          end
        '';
      };
    }
    {
      event = [
        "BufEnter"
      ];
      pattern = [
        "*.cs"
        "*.html"
        "*.js"
        "*.lua"
        "*.rs"
        "*.sh"
        "*.ts"
        "*.nix"
        "lua"
        "nix"
      ];
      callback = {
        __raw = ''
          function()
            local zm = require("zen-mode")
            local win = vim.api.nvim_get_current_win()
            local buffer_id = vim.api.nvim_win_get_buf(win)
            -- pcall(zm.close);
            -- pcall(zm.toggle, {
            --   window = { 
            --     width = 85,
            --     --height = 1,
            --   }
            -- })
            pcall(zm.close, {
              window = { 
                width = 85,
                --height = 1,
              }
            })
            vim.api.nvim_set_current_buf(buffer_id)
            pcall(zm.open, {
              window = { 
                width = 100,
                --height = 1,
              }
            })
          end
        '';
      };
    }
    # {
    #   event = [
    #     "BufLeave"
    #   ];
    #   pattern = [
    #     "*.cs"
    #     "*.html"
    #     "*.js"
    #     "*.lua"
    #     "*.markdown"
    #     "*.md"
    #     "*.rs"
    #     "*.sh"
    #     "*.ts"
    #     "*.txt"
    #     "gitcommit"
    #     "*.nix"
    #   ];
    #   callback = {
    #     __raw = ''
    #       function()
    #         local zm = require("zen-mode")
    #         pcall(zm.close);
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
