{ config, pkgs, ... }: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = [ "${config.home.homeDirectory}/.ssh/me" ];
  age.secrets.bw-cal-client-id.file = ./bw-cal-client-id.age;

  accounts.calendar.accounts."bitwarden".vdirsyncer.clientIdCommand =
    [ "${pkgs.coreutils}/bin/cat" "${config.age.secrets.bw-cal-client-id.path}" ];
}
