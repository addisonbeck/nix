{ ... }: {
  programs.gh-dash.enable = true;
  programs.gh-dash.settings = {
    repoPaths = {
      "addisonbeck/nix" = "~/nix";
      "bitwarden/server" = "~/bitwarden/server";
      "bitwarden/clients" = "~/bitwarden/clients";
      "bitwarden/directory-connector" = "~/bitwarden/directory-connector";
      "addisonbeck/d" = "~/d";
      "addisonbeck/binwarden" = "~/binwarden";
      "addisonbeck/notes" = "~/notes";
      "addisonbeck/recipes" = "~/recipes";
    };
    prSections = [
      {
        title = "My PRs (Open)";
        filters = "is:open author:@me";
        layout.author.hidden = true;
      }
      {
        title = "My PRs (All)";
        filters = "author:@me";
        layout.author.hidden = true;
      }
      {
        title = "Involved";
        filters = "is:open involves:@me";
      }
      {
        title = "Involved (Renovate)";
        filters = "is:open involves:@me author:app/renovate";
      }
      {
        title = "Needs My Review";
        filters = "is:open is:pr review-requested:addisonbeck archived:false ";
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
