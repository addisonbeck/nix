{pkgs, ...}: {
  networking = {
    firewall.allowedTCPPorts = [
      80 # HTTP
      443 # HTTPS
    ];
  };

  systemd.services.nginx = {
    requires = ["tailscale-cert.service"];
    after = ["tailscale-cert.service"];
  };

  systemd.tmpfiles.rules = [
    "d /etc/nginx/ssl 0755 root root -"
  ];

  systemd.services.nginx-init = {
    description = "Initialize Nginx SSL certificates";
    wantedBy = ["multi-user.target"];
    before = ["nginx.service"]; #
    script = ''
      if [ ! -f /etc/nginx/ssl/homelab.key ]; then
        ${pkgs.openssl}/bin/openssl req -x509 -newkey rsa:4096 \
          -keyout /etc/nginx/ssl/homelab.key \
          -out /etc/nginx/ssl/homelab.crt \
          -days 365 -nodes -subj '/CN=homelab'
      fi
      chown nginx:nginx /etc/nginx/ssl/homelab.key /etc/nginx/ssl/homelab.crt
      chmod 400 /etc/nginx/ssl/homelab.key
      chmod 444 /etc/nginx/ssl/homelab.crt
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
  };

  services.nginx = {
    user = "nginx";
    enable = true;
    recommendedProxySettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;

    virtualHosts = {
      "homelab" = {
        serverName = "homelab homelab.tail357e32.ts.net";
        forceSSL = true;
        sslCertificate = "/etc/nginx/ssl/homelab.tail357e32.ts.net.crt";
        sslCertificateKey = "/etc/nginx/ssl/homelab.tail357e32.ts.net.key";
        locations = {
          "/" = {
            return = ''
              200 '<!DOCTYPE html>
              <html>
              <head>
                <title>Homelab Dashboard</title>
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <style>
                  body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; margin: 0 auto; max-width: 800px; padding: 20px; color: #333; }
                  h1 { text-align: center; margin-bottom: 30px; }
                  .services { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 20px; }
                  .service { background: #f5f5f5; border-radius: 8px; padding: 20px; text-align: center; transition: transform 0.2s; }
                  .service:hover { transform: translateY(-5px); box-shadow: 0 5px 15px rgba(0,0,0,0.1); }
                  a { color: #0366d6; text-decoration: none; font-weight: bold; }
                </style>
              </head>
              <body>
                <h1>Homelab Dashboard</h1>
                <div class="services">
                  <div class="service"><a href="/adguard/">AdGuard Home</a></div>
                  <div class="service"><a href="/grafana/">Grafana</a></div>
                  <div class="service"><a href="/prometheus/">Prometheus</a></div>
                  <div class="service"><a href="/rss/">FreshRSS</a></div>
                  <div class="service"><a href="/vaultwarden/">Vaultwarden Web</a></div>
                  <div class="service"><a href="/vaultwarden/admin/">Vaultwarden Admin</a></div>
                </div>
              </body>
              </html>'
            '';
            extraConfig = ''
              add_header Content-Type text/html;
            '';
          };
        };
      };
    };
  };
}
