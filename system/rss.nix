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
        extraConfig = ''
          fastcgi_buffers 16 16k;
          fastcgi_buffer_size 32k;
        '';
        locations."~ \\.php$".extraConfig = ''
          fastcgi_pass unix:/run/phpfpm/freshrss;
          fastcgi_index index.php;
          include ${pkgs.nginx}/conf/fastcgi_params;
          fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
          fastcgi_param PATH_INFO $fastcgi_path_info;
        '';
    };

  };
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ];
  };

  services.phpfpm.pools.freshrss = {
    user = config.services.freshrss.user;
    settings = {
      "listen" = "/run/phpfpm/freshrss";
      "listen.owner" = config.services.nginx.user;
      "pm" = "dynamic";
      "pm.max_children" = 32;
      "pm.start_servers" = 2;
      "pm.min_spare_servers" = 2;
      "pm.max_spare_servers" = 5;
    };
  };

  users.users.root = {
    openssh.authorizedKeys.keyFiles = ["${authorizedKeys}"];
  };
}
