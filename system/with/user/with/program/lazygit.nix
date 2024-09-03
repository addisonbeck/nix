{ pkgs, ... }: {
  programs.lazygit.enable = true;
  #programs.lazygit.settings.pager = "${pkgs.delta}/bin/delta --dark --paging=never";
  programs.lazygit.settings = {
    git.paging.colorArg = "always";
    git.paging.pager = "${pkgs.delta}/bin/delta --dark --paging=never";
    gui.portraitMode = "auto";
    customCommands = [{
      key = "O";
      command = "nvim -c ':Octo pr create draft'";
      context = "localBranches";
      loadingText = "Loading Octo";
      description = "Open pull request with Octo as a draft";
      subprocess = true;
    }];
  };
}

