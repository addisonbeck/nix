{
  config,
  pkgs,
  ...
}: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/me"];
  age.secrets.email.file = ./email.age;

  accounts.calendar.accounts."addisonbeck_com".remote.passwordCommand = ["${pkgs.coreutils}/bin/cat" "${config.age.secrets.email.path}"];

  accounts.email.accounts."addisonbeck_com".passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.email.path}";
}
