{
  config,
  pkgs,
  ...
}: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/me"];
  age.secrets.bw-cal-client-secret.file = ./bw-cal-client-secret.age;

  accounts.calendar.accounts."bitwarden".vdirsyncer.clientSecretCommand = ["${pkgs.coreutils}/bin/cat" "${config.age.secrets.bw-cal-client-secret.path}"];
}
