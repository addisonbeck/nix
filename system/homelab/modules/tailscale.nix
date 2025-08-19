{pkgs, ...}: {
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
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
}
