{
  inputs,
  lib,
  pkgs,
  rootPath,
  ...
}: {
  # VM Configuration for Testing Bitwarden Client Regressions
  imports = [
    "${inputs.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
    inputs.home-manager.nixosModules.home-manager
    ./with/user/testing-user.nix
  ];

  # Basic system configuration
  system.stateVersion = "24.11";

  # Boot configuration
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Networking
  networking.hostName = "linux-testing";
  networking.networkmanager.enable = true;

  # Time zone and locale (matching Addison's location)
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";

  # KDE Plasma Desktop Environment
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

  # Graphics and hardware acceleration
  hardware.graphics.enable = true;
  # 32-bit graphics only supported on x86_64 systems
  hardware.graphics.enable32Bit = pkgs.stdenv.hostPlatform.isx86_64;

  # Audio
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    audio.enable = true;
    pulse.enable = true;
  };

  # Flatpak support for Bitwarden testing
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];

  # Development tools for building Bitwarden clients
  environment.systemPackages = with pkgs; [
    # Node.js ecosystem for client builds
    nodejs_22

    # Build essentials
    gcc
    gnumake
    pkg-config
    python3

    # Version control and development
    git
    gh
    jq

    # System utilities
    htop
    btop
    neofetch
    wget
    curl

    # Text editors
    vim

    # Browsers for testing
    firefox
    chromium

    # Terminal utilities
    fish
    kitty
    kdePackages.konsole
  ];

  # Enable development services
  services.openssh.enable = true;
  virtualisation.docker.enable = true;

  # QEMU VM specific configuration
  virtualisation = {
    memorySize = 8192; # 8GB RAM
    cores = 4; # 4 CPU cores
    diskSize = 40960; # 40GB disk
    resolution = {
      x = 1920;
      y = 1080;
    };
    qemu = {
      options = [
        # Enhanced graphics performance
        "-device virtio-vga-gl"
        "-display gtk,gl=on"
        # Better network performance
        "-netdev user,id=net0,hostfwd=tcp::2222-:22"
        "-device virtio-net,netdev=net0"
      ];
    };
  };

  # Security and polkit
  security.polkit.enable = true;
  security.sudo.wheelNeedsPassword = false; # For testing convenience

  # Allow unfree packages (needed for some development tools)
  nixpkgs.config.allowUnfree = true;

  # Enable fish shell system-wide
  programs.fish.enable = true;
}
