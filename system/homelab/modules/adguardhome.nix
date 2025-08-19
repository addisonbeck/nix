{
  config,
  pkgs,
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
  options = {};
  config = {
    networking = {
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
      firewall.allowedTCPPorts = [
        53 # DNS
        #67 # DHCP server
        #68 # DHCP client
        3000 # Web UI
      ];

      firewall.allowedUDPPorts = [
        53 # DNS
        #67  # DHCP server
        #68  # DHCP client
      ];
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
    systemd.services.adguard-exporter = {
      description = "AdGuard Exporter";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];
      serviceConfig = {
        ExecStart = "${adguard-exporter}/bin/adguard-exporter";
        User = "nobody";
        Restart = "always";
        EnvironmentFile = config.sops.secrets.adguard-env-file.path;
      };
      environment = {
        ADGUARD_SERVERS = "http://localhost:3000";
        ADGUARD_USERNAMES = "admin";
        INTERVAL = "30s";
      };
    };
    services.prometheus = {
      scrapeConfigs = [
        {
          job_name = "adguard-exporter";
          static_configs = [
            {
              targets = ["localhost:9618"];
            }
          ];
        }
      ];
    };

    services.nginx.virtualHosts = {
      "homelab-server".locations = {
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
      };
    };
  };
}
