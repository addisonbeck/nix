{...}: {
  sops.secrets.nasa-token = {
    format = "yaml";
    sopsFile = ../../../secrets/nasa-token.yaml;
    key = "token";
    mode = "0600";
  };
}
