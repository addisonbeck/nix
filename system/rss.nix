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
    #virtualHost = "rss.addisonbeck.dev";
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

        # php files handling
        # this regex is mandatory because of the API
        locations."~ ^.+?\.php(/.*)?$".extraConfig = ''
        fastcgi_pass unix:${config.services.phpfpm.pools.${config.services.freshrss.pool}.socket};
        fastcgi_split_path_info ^(.+\.php)(/.*)$;

        # By default, the variable PATH_INFO is not set under PHP-FPM
        # But FreshRSS API greader.php need it. If you have a “Bad Request” error, double check this var!
        # NOTE: the separate $path_info variable is required. For more details, see:
        # https://trac.nginx.org/nginx/ticket/321
        set $path_info $fastcgi_path_info;
        fastcgi_param PATH_INFO $path_info;
        include ${config.services.nginx.package}/conf/fastcgi_params;
        include ${config.services.nginx.package}/conf/fastcgi.conf;
        '';
    };

  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ];
  };

  services.phpfpm = {
    pools.freshrss = {
      user = config.services.freshrss.user;
      group = config.services.nginx.group;  
      settings = {
        "listen" = "/run/phpfpm/freshrss";
        "listen.owner" = config.services.nginx.user;
        "pm" = "dynamic";
        "pm.max_children" = 32;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 2;
        "pm.max_spare_servers" = 5;
        "php_admin_value[session.cookie_secure]" = "On";
        "php_admin_value[session.cookie_httponly]" = "On";
        "php_admin_value[session.save_handler]" = "files";
        "php_admin_value[session.save_path]" = "/tmp";
      };
    };
  };

  users.users."root".openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH me@bw"
  ];
}
