{...}: {
  sops.secrets."authorized_keys" = {
    format = "binary";
    sopsFile = ../../../secrets/authorized_keys;
    path = "/etc/.ssh/authorized_keys";
    mode = "0400";
  };
}
