{
  lib,
  pkgs,
  config,
  ...
}: {
  networking.hostName = lib.mkDefault "raspberrypi";
  networking.firewall.allowedTCPPorts = [22];

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIGZBzWy75jm3OlX3ACmzoRzMQgDeXLCDX94OuD2dgyCjAAAABHNzaDo= me@addisonbeck.com"
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAILkmHr6OFzrqcA5KggsJrM0B20DBIjON6rH9D3irObgsAAAABHNzaDo= me@addisonbeck.com"
  ];

  security.sudo = {
    enable = true;
    extraRules = [
      {
        commands = [
          {
            command = "ALL";
            options = ["NOPASSWD"];
          }
        ];
        groups = ["wheel"];
      }
    ];
  };

  # Use mainline kernel instead of rpi4 kernel for better stability
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nixpkgs.crossSystem = {
    system = "aarch64-linux";
    config = "aarch64-unknown-linux-gnu";
  };

  hardware.enableRedistributableFirmware = true;
  boot.kernelParams = ["console=ttyS0,115200n8" "console=ttyAMA0,115200n8" "console=tty0"];

  # GPU memory split for 2GB Pi 4 (reduces RAM pressure)
  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
    firmwareConfig = ''
      gpu_mem=16
      dtparam=audio=on
      # Reduce memory pressure on 2GB Pi
      disable_camera_led=1
      camera_auto_detect=0
      display_auto_detect=0
    '';
  };

  # Add more boot options for stability
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.initrd.availableKernelModules = lib.mkForce [
    "mmc_block"
    "sdhci"
    "sdhci_pci" 
    "sdhci_bcm2835"
    "bcm2835_dma"
    "i2c_bcm2835"
    "virtio_net"
    "virtio_pci"
    "virtio_mmio"
    "virtio_blk"
    "virtio_scsi"
    "virtio_balloon"
  ];
}

