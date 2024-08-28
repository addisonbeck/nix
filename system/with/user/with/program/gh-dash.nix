{ ... }: {
  programs.gh-dash.enable = true;
  programs.gh-dash.settings = {
    repoPaths = {
      "addisonbeck/nix" = "~/nix";
      "bitwarden/server" = "~/bitwarden/server";
      "bitwarden/clients" = "~/bitwarden/clients";
    };
    prSections = [
      {
        title = "My Pull Requests";
        filters = "is:open author:@me";
        layout.author.hidden = true;
      }
      {
        title = "Needs My Review";
        filters = "is:open is:pr review-requested:addisonbeck archived:false ";
      }
      {
        title = "Involved";
        filters = "is:open involves:@me -author:@me";
      }
    ];
    keybindings = {
      prs = [{
        key = "O";
        command = ''
          tmux new-window -c {{.RepoPath}} 'nvim -c ":Octo pr edit {{.PrNumber}}"';
	'';
      }];
    };
  };
}
