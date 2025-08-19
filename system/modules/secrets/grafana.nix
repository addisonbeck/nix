{...}: {
  sops.secrets.grafana-password = {
    format = "yaml";
    sopsFile = ../../../secrets/grafana.yaml;
    key = "password";
    mode = "0600";
  };
}
