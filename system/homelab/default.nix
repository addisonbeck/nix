{ ... }: {
  imports = [
    ../raspberrypiimage.nix
    ../with/trait/has-swapfile.nix
    {has-swapfile.sizeGb = 2;}
    ./modules/system-boot.nix
    ./modules/filesystems.nix
    ./modules/tailscale.nix
    ./modules/prometheus.nix
    ./modules/grafana.nix
    ./modules/adguardhome.nix
    ./modules/freshrss.nix
    ./modules/vaultwarden.nix
    ./modules/mutt.nix
    ./modules/calibre/default.nix
    ./modules/nginx.nix
  ];
  system.stateVersion = "24.11";
}
