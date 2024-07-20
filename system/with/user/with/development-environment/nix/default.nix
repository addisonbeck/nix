{
  config,
  ...
}: 
{
  imports = [
    ../../service/autoclone.nix
    {
      service.autoclone = {
        enable = true;
        name = "nix";
        url = "git@github.com:addisonbeck/nix.git";
        save-path = "/home/me/nix";
        ssh-key = "${config.age.secrets.github.path}";
      };
    }
  ];
  programs.direnv.config.whitelist.exact =
    [ "${config.home.homeDirectory}/nix" ];
}
