{config, ...}: {
  services.autoclone.repo.notes = {
    enable = true;
    url = "git@github.com:addisonbeck/notes.git";
    save-path = "${config.home.homeDirectory}/notes";
    ssh-key = "${config.age.secrets.github.path}";
  };
  programs.direnv.config.whitelist.exact = ["${config.home.homeDirectory}/notes"];
}
