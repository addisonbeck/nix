{...}: {
  sops.secrets.freshrss = {
    format = "yaml";
    sopsFile = ../../../secrets/freshrss.yaml;
    key = "password";
    mode = "0444";
    owner = "freshrss";
  };
}
