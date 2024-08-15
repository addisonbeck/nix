{ config, ... }: {
  # For files and scripts that are still managed through the `d` cli
  home.sessionPath = [ "/Users/me/binwarden2" ];

  services.autoclone.repo.binwarden = {
    enable = true;
    url = "git@github.com:addisonbeck/binwarden.git";
    save-path = "${config.home.homeDirectory}/binwarden2";
    ssh-key = "${config.age.secrets.github.path}";
  };
}
