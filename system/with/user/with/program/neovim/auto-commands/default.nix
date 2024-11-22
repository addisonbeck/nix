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
    # {
    #   event = [
    #     "BufEnter"
    #   ];
    #   pattern = [
    #     "*.markdown"
    #     "*.md"
    #     "*.txt"
    #     "gitcommit"
    #     "markdown"
    #     "md"
    #     "*.cs"
    #     "*.html"
    #     "*.js"
    #     "*.lua"
    #     "*.rs"
    #     "*.sh"
    #     "*.ts"
    #     "*.nix"
    #     "lua"
    #     "nix"
    #   ];
    #   callback = {
    #     __raw = ''
    #       function()
    #         local zm = require("zen-mode")
    #         pcall(zm.open, {
    #           window = { 
    #             width = 85,
    #             height = 0.95,
    #           }
    #         })
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
