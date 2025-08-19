{config, ...}: {
  networking = {
    firewall.allowedTCPPorts = [
      8222
    ];
  };

  systemd.tmpfiles.rules = [
    "d /var/srv 0750 vaultwarden vaultwarden -"
    "d /var/srv/vaultwarden 0750 vaultwarden vaultwarden -"
    "d /var/srv/vaultwarden/data 0750 vaultwarden vaultwarden -"
    "d /var/srv/vaultwarden/backup 0750 vaultwarden vaultwarden -"
  ];

  services.vaultwarden = {
    enable = true;
    backupDir = "/var/srv/vaultwarden/backup";

    environmentFile = config.sops.secrets.vaultwarden-env-file.path;
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

  services.nginx = {
    virtualHosts = {
      "homelab-server" = {
        locations = {
          "/vaultwarden" = {
            proxyPass = "http://127.0.0.1:8222";
            proxyWebsockets = true;
            extraConfig = ''
              add_header X-Robots-Tag "none";
            '';
          };
        };
      };
    };
  };
}
