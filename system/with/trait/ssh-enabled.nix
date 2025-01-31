{...}: {
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";
  services.openssh.settings.PasswordAuthentication = false;
  networking.firewall.allowedTCPPorts = [22];
}
