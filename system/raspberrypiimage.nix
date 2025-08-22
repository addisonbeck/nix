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

  boot.kernelPackages = pkgs.linuxPackages_rpi4;

  nixpkgs.crossSystem = {
    system = "aarch64-linux";
    config = "aarch64-unknown-linux-gnu";
  };

  hardware.enableRedistributableFirmware = true;
  boot.kernelParams = ["console=ttyS0,115200n8" "console=ttyAMA0,115200n8" "console=tty0"];
  boot.initrd.availableKernelModules = lib.mkForce [
    "virtio_net"
    "virtio_pci"
    "virtio_mmio"
    "virtio_blk"
    "virtio_scsi"
    "virtio_balloon"
  ];
}
