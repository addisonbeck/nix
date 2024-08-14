{ config, ... }: {
  # For files and scripts that are still managed through the `d` cli
  home.sessionPath = [ "/Users/me/binwarden" ];

  services.autoclone.repo.dotfiles = {
    enable = true;
    url = "git@github.com:addisonbeck/dotfiles.git";
    save-path = "${config.home.homeDirectory}/binwarden";
    ssh-key = "${config.age.secrets.github.path}";
  };
}
