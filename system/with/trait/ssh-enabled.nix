{...}: {
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "no";
  services.openssh.settings.PasswordAuthentication = false;
  services.openssh.firewall.KbdInteractiveAuthentication = false;  
  networking.firewall.allowedTCPPorts = [22];
}
