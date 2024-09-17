{
  config,
  pkgs,
  ...
}: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/me"];
  age.secrets.bw-mail-password.file = ./bw-mail-password.age;

  accounts.email.accounts."bitwarden".passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.bw-mail-password.path}";
}
