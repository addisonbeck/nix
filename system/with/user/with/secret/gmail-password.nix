{
  config,
  pkgs,
  ...
}: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/me"];
  age.secrets.gmail-password.file = ./gmail-password.age;

  accounts.calendar.accounts."gmail".remote.passwordCommand = ["${pkgs.coreutils}/bin/cat" "${config.age.secrets.gmail-password.path}"];

  accounts.email.accounts."gmail".passwordCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.gmail-password.path}";
}
