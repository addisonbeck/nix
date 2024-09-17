{config, ...}: {
  services.autoclone.repo.bitwarden-server = {
    enable = true;
    url = "git@github.com:bitwarden/server.git";
    save-path = "${config.home.homeDirectory}/bitwarden/server";
    ssh-key = "${config.age.secrets.github.path}";
  };
  services.autoclone.repo.bitwarden-clients = {
    enable = true;
    url = "git@github.com:bitwarden/clients.git";
    save-path = "${config.home.homeDirectory}/bitwarden/clients";
    ssh-key = "${config.age.secrets.github.path}";
  };
  services.autoclone.repo.bitwarden-directory-connector = {
    enable = true;
    url = "git@github.com:bitwarden/directory-connector.git";
    save-path = "${config.home.homeDirectory}/bitwarden/directory-connector";
    ssh-key = "${config.age.secrets.github.path}";
  };
  programs.direnv.config.whitelist.exact = [
    "${config.home.homeDirectory}/bitwarden/server"
    "${config.home.homeDirectory}/bitwarden/clients"
    "${config.home.homeDirectory}/bitwarden/directory-connector"
  ];
}
