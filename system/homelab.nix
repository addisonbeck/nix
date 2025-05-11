{
  pkgs,
  config,
  lib,
  ...
}: let
  adguard-exporter = pkgs.buildGoModule rec {
    pname = "adguard-exporter";
    version = "1.2.0";

    src = pkgs.fetchFromGitHub {
      owner = "henrywhitaker3";
      repo = "adguard-exporter";
      rev = "v${version}";
      sha256 = "sha256-HQ72eTBV6ZZMawACZ/iwABVm8On/yqFKotfXxbGZqUc=";
    };
    vendorHash = "sha256-18Fzld2F05hzORw8kLt6JdCdxGNM+KOEUiPnC3LTzDI=";

    meta = with lib; {
      description = "Prometheus exporter for AdGuard Home";
      homepage = "https://github.com/henrywhitaker3/adguard-exporter";
      license = licenses.mit;
      maintainers = with maintainers; [];
    };
  };
in {
  imports = [
    ./raspberrypiimage.nix
    ./with/trait/has-swapfile.nix
    ./with/user/with/secret/freshrss.nix
    {has-swapfile.sizeGb = 2;}
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking = {
    hostName = "homelab";

    /*
    Untoggle this if you ever get DHCP working
    */
    interfaces.end0 = {
      useDHCP = false;
      ipv4.addresses = [
        {
          address = "192.168.1.45";
          prefixLength = 24;
        }
      ];
    };

    defaultGateway = "192.168.1.1";
    nameservers = ["127.0.0.1"];
    firewall.allowedTCPPorts = [
      53 # DNS
      #67 # DHCP server
      #68 # DHCP client
      80 # HTTP
      443 # HTTPS
      3000 # AdGuard
      3001 # Grafana
      8222 # Vaultwarden
    ];

    firewall.allowedUDPPorts = [
      53 # DNS
      #67  # DHCP server
      #68  # DHCP client
    ];
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };

  services.freshrss = {
    enable = true;
    defaultUser = "me";
    passwordFile = config.age.secrets.freshrss.path;
    baseUrl = "https://homelab/rss/";
    virtualHost = "homelab";
    extensions = [pkgs.freshrss-extensions.youtube];
  };

  systemd.services.tailscale-cert = {
    description = "Tailscale certificate renewal";
    wantedBy = ["multi-user.target"];
    after = ["tailscaled.service" "network-online.target"];
    before = ["nginx.service"];
    path = [pkgs.bash pkgs.coreutils pkgs.tailscale];

    script = ''
      # Generate the Tailscale certificate
      tailscale cert homelab.tail357e32.ts.net

      # Copy to nginx directory with proper permissions
      cp /var/lib/tailscale/certs/homelab.tail357e32.ts.net.crt /etc/nginx/ssl/
      cp /var/lib/tailscale/certs/homelab.tail357e32.ts.net.key /etc/nginx/ssl/

      # Set proper ownership and permissions
      chown nginx:nginx /etc/nginx/ssl/homelab.tail357e32.ts.net.*
      chmod 644 /etc/nginx/ssl/homelab.tail357e32.ts.net.crt
      chmod 600 /etc/nginx/ssl/homelab.tail357e32.ts.net.key
    '';

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
  };

  systemd.timers.tailscale-cert = {
    description = "Regularly renew Tailscale certificate";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "1m";
      OnUnitActiveSec = "30d";
      Unit = "tailscale-cert.service";
    };
  };

  services.adguardhome = {
    enable = true;
    openFirewall = true; # Not really sure what ports this opens
    mutableSettings = true;
    port = 3000; # The default
    settings = {
      users = [
        {
          name = "admin";
          password = "$2y$12$HOzxaWNByWsfJC4WKcd1UO/6bj3AvDzoJ7P.aEVCTCsAhw6nBCeJ2";
        }
      ];
      theme = "auto";
      dns = {
        /*
        Disables EDNS Client Subnet, a DNS extension that shares part of
        your IP address subnet with DNS servers and CDNs. Reduceses
        accuracy of malicious location tracking over the network.
        */
        edns_client_subnet.enabled = false;
        cache_size = 536870912; # 512 MB
        cache_ttl_min = 1800; # 30 min
        cache_optimistic = true; # return stale and then refresh
        # All upstream dns is pointed to quad9
        upstream_dns = [
          /*
          DNS is over HTTPs (DoH). This is encrypted, bypasses most
          filtering, and hides HTTPs traffic. This adds a small layer of
          privacy between our network and our ISP.

          Other protocols for quad9 would be
          regular: 9.9.9.9
          DNS over TLS: tls://dns.quad9.net
          */
          "https://dns10.quad9.net/dns-query"
        ];

        # Uses the quad9 "secure" variant". ipv4 and 6.
        bootstrap_dns = ["9.9.9.10" "149.112.112.10" "2620:fe::10" "2620:fe::fe:10"];
      };
      protection_enabled = true;
      filtering = {
        protection_enabled = true;
        filtering_enabled = true;
        blocked_services = {
          schedule = {
            time_zone = "Local";
          };
          ids = [
            "4chan"
            "9gag"
            "discord"
            "kik"
            "onlyfans"
            "skype"
            "snapchat"
            "telegram"
            "tinder"
            "tumblr"
            "twitter"
            "viber"
            "wechat"
            "whatsapp"
          ];
        };
      };
      filters = [
        {
          enabled = true;
          url = "https://adguardteam.github.io/AdGuardSDNSFilter/Filters/filter.txt";
          name = "AdGuard DNS filter";
          id = 1;
        }
        {
          enabled = true;
          url = "https://easylist.to/easylist/easylist.txt";
          name = "EasyList";
          id = 2;
        }
        {
          enabled = true;
          url = "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts";
          name = "StevenBlack's unified hosts w/ extensions";
          id = 3;
        }
        {
          enabled = true;
          url = "https://big.oisd.nl";
          name = "OISD Big";
          id = 4;
        }
        {
          enabled = true;
          url = "https://nsfw.oisd.nl";
          name = "OISD NSFW";
          id = 5;
        }
      ];
      stats = {
        enabled = true;
        interval = "24h";
      };
      dhcp = {
        # I want to, but the current router won't let me.
        # Just need to set this to true though and things should "just work" on .10.
        enabled = false;
        interface_name = "end0";
        /*
        LAN Ip of the router
        If this changes (I don't think it will) things will break
        Can be found on the deco admin panel
        */
        dhcpv4 = {
          gateway_ip = "192.168.1.1";
          subnet_mask = "255.255.255.0";
          range_start = "192.168.1.20";
          range_end = "192.168.1.250";
          lease_duration = 86400; # 24 hours
        };
      };
    };
  };

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
      {
        job_name = "adguard-exporter";
        static_configs = [
          {
            targets = ["localhost:9618"];
          }
        ];
      }
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

  age.secrets.homelab-grafana-admin-password = {
    file = ./with/user/with/secret/homelab-grafana-admin-password.age;
    owner = "grafana";
  };

  age.secrets.homelab-adguard-admin-password = {
    file = ./with/user/with/secret/homelab-adguard-admin-password.age;
  };

  age.secrets.homelab-vaultwarden-env-file = {
    file = ./with/user/with/secret/homelab-vaultwarden-env-file.age;
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

      /*
      The =$__file{path}= syntax is a Grafana-specific feature that tells
      Grafana to read the password from the file path. This way, your
      secret file can just contain the raw password without any formatting
      requirements.
      */
      security.admin_password = "$__file{${config.age.secrets.homelab-grafana-admin-password.path}}";
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

  systemd.services.adguard-exporter = {
    description = "AdGuard Exporter";
    wantedBy = ["multi-user.target"];
    after = ["network.target"];
    serviceConfig = {
      ExecStart = "${adguard-exporter}/bin/adguard-exporter";
      User = "nobody";
      Restart = "always";
      EnvironmentFile = config.age.secrets.homelab-adguard-admin-password.path;
    };
    environment = {
      ADGUARD_SERVERS = "http://localhost:3000";
      ADGUARD_USERNAMES = "admin";
      INTERVAL = "30s";
    };
  };

  systemd.tmpfiles.rules = [
    "d /etc/nginx/ssl 0755 root root -"
    "d /var/srv 0750 vaultwarden vaultwarden -"
    "d /var/srv/vaultwarden 0750 vaultwarden vaultwarden -"
    "d /var/srv/vaultwarden/data 0750 vaultwarden vaultwarden -"
    "d /var/srv/vaultwarden/backup 0750 vaultwarden vaultwarden -"
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

  services.vaultwarden = {
    enable = true;
    backupDir = "/var/srv/vaultwarden/backup";

    environmentFile = config.age.secrets.homelab-vaultwarden-env-file.path;
    config = {
      ROCKET_ADDRESS = "127.0.0.1";
      ROCKET_PORT = 8222;
      WEBSOCKET_ENABLED = true;
      # SIGNUPS_ALLOWED = false;
      # SIGNUPS_VERIFY = false;
      DOMAIN = "https://homelab.tail357e32.ts.net/vaultwarden/";
      WEB_VAULT_ENABLED = true;
      SMTP_HOST = "box.addisonbeck.com";
      SMTP_FROM = "vaultwarden@addisonbeck.com";
      SMTP_FROM_NAME = "Vaultwarden";
      SMTP_USERNAME = "vaultwarden@addisonbeck.com";
      SMTP_TIMEOUT = "15";
      SMTP_SECURITY = "force_tls";
    };
  };

  security = {
    acme = {
      acceptTerms = true;
      defaults.email = "acme@addisonbeck.com";
    };
    sudo.wheelNeedsPassword = false;
  };

  services.nginx = {
    user = "nginx";
    enable = true;
    recommendedProxySettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;

    virtualHosts = {
      "homelab-server" = {
        serverName = "homelab homelab.tail357e32.ts.net";
        forceSSL = true;
        sslCertificate = "/etc/nginx/ssl/homelab.tail357e32.ts.net.crt";
        sslCertificateKey = "/etc/nginx/ssl/homelab.tail357e32.ts.net.key";
        locations = {
          "/adguard/" = {
            extraConfig = ''
              proxy_pass http://127.0.0.1:3000/;
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_redirect / /adguard/;
            '';
          };
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
          "/vaultwarden" = {
            proxyPass = "http://127.0.0.1:8222";
            proxyWebsockets = true;
            extraConfig = ''
              add_header X-Robots-Tag "none";
            '';
          };

          "/rss/" = {
            alias = "${pkgs.freshrss}/p/";
            index = "index.php";
            tryFiles = "$uri $uri/ /rss/index.php$is_args$args";
          };

          "~ ^/rss/(.+\.php)(/.*)?$" = {
            extraConfig = ''
              fastcgi_pass unix:${config.services.phpfpm.pools."freshrss".socket};
              include ${pkgs.nginx}/conf/fastcgi_params;
              include ${pkgs.nginx}/conf/fastcgi.conf;

              # Critical fix: Set the proper script filename with full path
              fastcgi_param SCRIPT_FILENAME ${pkgs.freshrss}/p/$1;

              # Handle path info correctly for API endpoints
              fastcgi_split_path_info ^/rss/(.+\.php)(/.*)?$;
              fastcgi_param PATH_INFO $fastcgi_path_info;
            '';
          };
          "/grafana/" = {
            proxyWebsockets = true;
            extraConfig = ''
              proxy_pass http://127.0.0.1:3001/;
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            '';
          };

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

  systemd.services.nginx = {
    requires = ["tailscale-cert.service"];
    after = ["tailscale-cert.service"];
  };

  system.stateVersion = "24.11";
}
