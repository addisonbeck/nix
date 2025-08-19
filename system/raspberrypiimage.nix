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
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH me@bw"
  ];

  users.users.root.openssh.authorizedKeys.keyFiles = [
    config.sops.secrets."authorized_keys".path
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
