{config, ...}: {
  home.sessionPath = ["/Users/me/bin/binwarden"];

  services.autoclone.repo.bitwarden-binwarden = {
    enable = true;
    url = "git@github.com:addisonbeck/binwarden.git";
    save-path = "${config.home.homeDirectory}/binwarden";
    ssh-key = "${config.age.secrets.github.path}";
  };
}
