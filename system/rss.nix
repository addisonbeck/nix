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
          root = "${pkgs.freshrss}/p";

          # php files handling
          # this regex is mandatory because of the API
          locations."~ ^.+?\\.php(/.*)?$".extraConfig = ''
            fastcgi_pass unix:${config.services.phpfpm.pools."freshrss".socket};
            fastcgi_split_path_info ^(.+\.php)(/.*)$;
            # By default, the variable PATH_INFO is not set under PHP-FPM
            # But FreshRSS API greader.php need it. If you have a “Bad Request” error, double check this var!
            # NOTE: the separate $path_info variable is required. For more details, see:
            # https://trac.nginx.org/nginx/ticket/321
            set $path_info $fastcgi_path_info;
            fastcgi_param PATH_INFO $path_info;
            include ${pkgs.nginx}/conf/fastcgi_params;
            include ${pkgs.nginx}/conf/fastcgi.conf;
          '';

          locations."/" = {
            tryFiles = "$uri $uri/ index.php";
            index = "index.php index.html index.htm";
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
