{...}: {
  programs.git.enable = true;
  programs.git.delta.enable = true;
  programs.git.extraConfig = {
    github.user = "addisonbeck";
  };
}
