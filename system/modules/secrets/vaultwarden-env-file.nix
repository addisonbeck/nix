{...}: {
  sops.secrets.vaultwarden-env-file = {
    format = "binary";
    sopsFile = ../../../secrets/vaultwarden-env-file;
    path = "/run/secrets/vaultwarden-env-file";
    mode = "0600";
  };
}
