{ pkgs, ... }: {
  config = {
    services.xserver.enable = true;
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;
    environment.gnome.excludePackages =
      (with pkgs; [ gnome-photos gnome-tour gedit cheese epiphany yelp geary ])
      ++ (with pkgs.gnome; [
        gnome-music
        gnome-characters
        tali
        iagno
        hitori
        atomix
        gnome-contacts
        gnome-initial-setup
      ]);
    programs.dconf.enable = true;
    environment.systemPackages = with pkgs; [ gnome-tweaks ];
  };
}
