{ config, pkgs, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
    ./with/nix.nix
    ./with/trait/ssh-enabled.nix
    ./with/trait/has-swapfile.nix
    {has-swapfile.sizeGb = 2;}
    ./with/user/root.nix
    ./with/user/with/secret/freshrss.nix
    {
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
    }
  ];

  services.freshrss = {
    enable = true;
    defaultUser = "me";
    passwordFile = config.age.secrets.freshrss.path;
    virtualHost = "rss.addisonbeck.dev";
    baseUrl = "https://rss.addisonbeck.dev";
  };

  services.nginx.virtualHosts."rss.addisonbeck.dev" = {
    addSSL = true;
    enableACME = true;
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "acme@addisonbeck.com";
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 443 ];
  };

  users.users."root".openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH me@bw"
  ];
}
