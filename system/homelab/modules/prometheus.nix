{
  config,
  pkgs,
  ...
}: {
  services.prometheus = {
    enable = true;
    listenAddress = "127.0.0.1";
    webExternalUrl = "https://homelab/prometheus";
    exporters = {
      node = {
        enable = true;
        enabledCollectors = ["systemd"];
        port = 9100;
      };
      systemd = {
        enable = true;
        port = 9101;
      };
    };
    scrapeConfigs = [
      {
        job_name = "node";
        static_configs = [
          {
            targets = ["localhost:9100"];
          }
        ];
      }
    ];
  };
  services.nginx = {
    virtualHosts = {
      "homelab-server" = {
        locations = {
          "/prometheus/" = {
            proxyWebsockets = true;
            extraConfig = ''
              proxy_pass http://127.0.0.1:9090/prometheus/;
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              #proxy_redirect off;
            '';
          };
        };
      };
    };
  };
}
