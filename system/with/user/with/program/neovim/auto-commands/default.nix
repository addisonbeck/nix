{...}: {
  autoCommands = [
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
        "FileType"
      ];
      pattern = [
        "md"
        "markdown"
        "txt"
        "gitcommit"
      ];
      callback = {
        __raw = ''
          function()
          vim.opt_local.wrap = true
          vim.keymap.set({'n', 'v'}, 'j', 'gj', {buffer = true})
          vim.keymap.set({'n', 'v'}, 'k', 'gk', {buffer = true})
          end
        '';
      };
    }
    {
      event = ["BufRead"];
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
