{...}: {
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "no";
  services.openssh.settings.PasswordAuthentication = false;
  networking.firewall.allowedTCPPorts = [22];
}
