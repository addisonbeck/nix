{ ... }: {
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  time.timeZone = "America/New_York";

  networking = {
    hostName = "homelab";
    defaultGateway = "192.168.1.1";
    nameservers = ["127.0.0.1"];
  };

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };

  security = {
    acme = {
      acceptTerms = true;
      defaults.email = "acme@addisonbeck.com";
    };
    sudo.wheelNeedsPassword = false;
  };
}
