{ config, ... }: {
  services.autoclone.repo.nix = {
    enable = true;
    url = "git@github.com:addisonbeck/nix.git";
    save-path = "${config.home.homeDirectory}/nix";
    ssh-key = "${config.age.secrets.github.path}";
  };
}
