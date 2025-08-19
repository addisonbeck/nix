{ config, ... }:
{
  sops.secrets.authinfo = {
    format = "binary";
    sopsFile = ../../../secrets/authinfo;
    # TODO Unset this and point emacs to the right place in .config
    path = "${config.home.homeDirectory}/.authinfo";
    mode = "0600";
  };
}
