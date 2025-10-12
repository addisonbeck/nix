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
          location ~ \.php$ {
            fastcgi_pass unix:${config.services.phpfpm.pools."freshrss".socket};
            include ${pkgs.nginx}/conf/fastcgi_params;
            fastcgi_param SCRIPT_FILENAME $request_filename;
            fastcgi_param PATH_INFO $fastcgi_path_info;
          }
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
