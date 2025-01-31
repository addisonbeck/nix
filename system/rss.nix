{ config, pkgs, modulesPath, ... }:
let
    authorizedKeys = pkgs.writeText "authorized_keys" ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH me@bw
  '';
in
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
  security.acme = {
    acceptTerms = true;
    defaults.email = "acme@addisonbeck.com";
  };
  services.nginx = {
    enable = true;
    virtualHosts."rss.addisonbeck.dev" = {
        enableACME = true;
        forceSSL = true;
    };
  };
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ];
  };

  services.phpfpm.pools.freshrss = {
    user = "freshrss";
    settings = {
      "pm" = "dynamic";
      "pm.max_children" = 32;
      "pm.start_servers" = 2;
      "pm.min_spare_servers" = 2;
      "pm.max_spare_servers" = 4;
      "php_admin_value[session.cookie_secure]" = "On";
      "php_admin_value[session.cookie_httponly]" = "On";
    };
  };

  services.openssh.authorizedKeys.keyFiles = ["${authorizedKeys}"];
}
