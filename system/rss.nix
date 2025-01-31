{ config, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
    ./with/nix.nix
    ./with/trait/ssh-enabled.nix
    ./with/trait/has-swapfile.nix
    {has-swapfile.sizeGb = 2;}
    ./with/user/root.nix
    ./with/user/with/secret/freshrss.nix
    {
      security.sudo = {
        enable = true;
        extraRules = [
          {
            commands = [
              {
                command = "ALL";
                options = ["NOPASSWD"];
              }
            ];
            groups = ["wheel"];
          }
        ];
      };
    }
  ];
  services.freshrss = {
    enable = true;
    defaultUser = "me";
    passwordFile = config.age.secrets.freshrss.path;
    virtualHost = "rss.addisonbeck.dev";
    baseUrl = "https://rss.addisonbeck.dev";
    database = {
      type = "sqlite";
    };
  };
  services.nginx = {
    enable = true;
    virtualHosts."rss.addisonbeck.dev" = {
        enableACME = true;
        forceSSL = true;
    };
  };
}
