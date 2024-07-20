{
  config,
  ...
}: {
  programs.git.enable = true;
  programs.git.extraConfig.gpg.format = "ssh";
  programs.git.signing.key = config.age.secrets.github.path;
  programs.git.signing.signByDefault = true;
  programs.git.userName = "Addison Beck";
  programs.git.userEmail = "github@addisonbeck.com";
  programs.git.extraConfig.pull.rebase = true;
}
