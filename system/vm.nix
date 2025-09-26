{
  inputs,
  lib,
  pkgs,
  ...
}: {
  # Basic system configuration
  system.stateVersion = "24.05";
  
  # Boot configuration
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
  # Filesystem configuration
  fileSystems."/" = {
    device = "/dev/sda1";  # Common VM root device
    fsType = "ext4";
  };
  
  # Networking
  networking.hostName = "vm";
  networking.networkmanager.enable = true;
  
  # Time zone and locale
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  
  # Enable basic services
  security.polkit.enable = true;
  services.dbus.enable = true;
  
  # Graphics and desktop
  hardware.graphics.enable = true;
  programs.hyprland.enable = true;
  
  # Audio
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    audio.enable = true;
    pulse.enable = true;
  };
  
  # Essential packages
  environment.systemPackages = with pkgs; [
    firefox
    emacs
    kitty
    wofi
  ];
  
  # Import user configuration
  imports = [
    inputs.home-manager.nixosModules.home-manager
    ./with/user/vm-me.nix
  ];
}
