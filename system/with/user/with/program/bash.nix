{...}: {
  programs.bash.enable = true;
  programs.bash.shellAliases = {
    bobert = "nix run /Users/me/nix/bobert --";
    claude = "nix run /Users/me/nix/bobert --";
    bobert-with-emacs = "nix run /Users/me/nix/bobert#bobert-with-emacs --";
    bobert-view = "nix run /Users/me/nix/bobert#bobert-view --";
  };
}
