{
  pkgs,
  config,
  ...
}: {
  services.freshrss = {
    enable = true;
    defaultUser = "me";
    passwordFile = config.sops.secrets.freshrss.path;
    baseUrl = "https://homelab/rss/";
    virtualHost = "homelab";
    extensions = [pkgs.freshrss-extensions.youtube];
  };
  services.nginx.virtualHosts = {
    "homelab".locations = {
      "/rss/" = {
        alias = "${pkgs.freshrss}/p/";
        index = "index.php";
        tryFiles = "$uri $uri/ /index.php$is_args$args";  # Changed this line
        extraConfig = ''
          # php files handling
          # this regex is mandatory because of the API
          location ~ ^.+?\.php(/.*)?$ {
            fastcgi_pass unix:/var/run/php/php8.1-fpm.sock;
            fastcgi_split_path_info ^(.+\.php)(/.*)$;
            # By default, the variable PATH_INFO is not set under PHP-FPM
            # But FreshRSS APIs greader.php and misc.php need it. If you have a “Bad Request” error, double check this var!
            # NOTE: the separate $path_info variable is required. For more details, see:
            # https://trac.nginx.org/nginx/ticket/321
            set $path_info $fastcgi_path_info;
            fastcgi_param PATH_INFO $path_info;
            include fastcgi_params;
            fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
        '';
    };
    "~ ^/rss/(.+\\.php)(/.*)?$" = {
      extraConfig = ''
        fastcgi_pass unix:${config.services.phpfpm.pools."freshrss".socket};
        include ${pkgs.nginx}/conf/fastcgi_params;
        include ${pkgs.nginx}/conf/fastcgi.conf;
        fastcgi_param SCRIPT_FILENAME ${pkgs.freshrss}/p/$1;
        fastcgi_split_path_info ^/rss/(.+\\.php)(/.*)?$;
        fastcgi_param PATH_INFO $fastcgi_path_info;
      '';
    };
  };
};
}
