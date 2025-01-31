{ config, pkgs, modulesPath, ... }:
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
    baseUrl = "https://rss.addisonbeck.dev";
    virtualHost = "rss.addisonbeck.dev";
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "rss.addisonbeck.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."~ ^/rss.addisonbeck.dev/.+?\.php(/.*)?$"={
            root = "${pkgs.freshrss}/p";
            extraConfig = ''
              fastcgi_pass unix:${config.services.phpfpm.pools.freshrss.socket};
              fastcgi_split_path_info ^/l_path}(/.+\.php)(/.*)?$;
              set $path_info $fastcgi_path_info;
              fastcgi_param PATH_INFO $path_info;
              fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
              include ${pkgs.nginx}/conf/fastcgi_params;
              include ${pkgs.nginx}/conf/fastcgi.conf;
         '';
          locations."~ ^/rss.addisonbeck.dev(?!.*\.php)(/.*)?$" = {
            root = "${pkgs.freshrss}/p";
            tryFiles = "$1 /rss.addisonbeck.dev$1/index.php$is_args$args";
            index = "index.php index.html index.htm";
          };
        };
      };
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "acme@addisonbeck.com";
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ];
  };

  users.users."root".openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH me@bw"
  ];
}
