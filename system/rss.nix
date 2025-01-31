{ inputs, modulesPath, pkgs, ... }:
{
  imports = [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
    inputs.home-manager.nixosModules.home-manager
    ./with/nix.nix
    ./with/trait/ssh-enabled.nix
    ./with/trait/has-swapfile.nix
    {has-swapfile.sizeGb = 2;}
    ./with/user/root.nix
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
    virtualHost = "rss.addisonbeck.dev";
    baseUrl = "https://rss.addisonbeck.dev";
    database = {
      type = "sqlite";
    };
  };
  services.nginx.virtualHosts."rss.addisonbeck.dev" = {
    enableACME = false;
    forceSSL = true;
    useACMEHost = "rss.addisonbeck.dev";
  };
}
