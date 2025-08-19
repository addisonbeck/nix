{config, ...}: {
  networking = {
    firewall.allowedTCPPorts = [
      3001
    ];
  };

  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "127.0.0.1";
        domain = "localhost";
        http_port = 3001;
        root_url = "https://homelab/grafana/";
      };
      security.admin_password = "$__file{${config.sops.secrets.grafana-password.path}}";
    };
    provision = {
      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          access = "proxy";
          url = "http://localhost:9090/prometheus";
          isDefault = true;
        }
      ];
    };
  };

  services.nginx.virtualHosts = {
    "homelab-server".locations = {
      "/grafana/" = {
        proxyWebsockets = true;
        extraConfig = ''
          proxy_pass http://127.0.0.1:3001/;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        '';
      };
    };
  };

  services.prometheus = {
    scrapeConfigs = [
      {
        job_name = "grafana";
        static_configs = [
          {
            targets = ["localhost:3001"];
          }
        ];
        metrics_path = "/metrics";
      }
    ];
  };
}
