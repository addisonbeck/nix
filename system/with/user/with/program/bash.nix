{...}: {
  programs.bash.enable = true;
  programs.bash.shellAliases = {
    bobert = "nix run /Users/me/nix/bobert --";
    claude = "nix run /Users/me/nix/bobert --";
  };
}
