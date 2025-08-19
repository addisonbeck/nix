{...}: {
  sops.secrets.adguard-env-file = {
    format = "binary";
    sopsFile = ../../../secrets/adguard-env-file;
    path = "/run/secrets/adguard-env-file";
    mode = "0600";
  };
}
