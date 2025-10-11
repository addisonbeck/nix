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

  # Use stable kernel to avoid ZFS compatibility issues
  boot.kernelPackages = pkgs.linuxPackages_6_6;

  nixpkgs.crossSystem = {
    system = "aarch64-linux";
    config = "aarch64-unknown-linux-gnu";
  };

  hardware.enableRedistributableFirmware = true;
  
  # Boot loader configuration and console params already provided by base SD module
  boot.initrd.availableKernelModules = lib.mkForce [
    "mmc_block"
    "sdhci"
    "sdhci_pci" 
    "sdhci_iproc"    # Mainline kernel uses sdhci_iproc for Pi SD
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

