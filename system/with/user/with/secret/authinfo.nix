{config, ...}: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/me"];
  age.secrets.authinfo.file = ./authinfo.age;
  age.secrets.authinfo.path = "${config.home.homeDirectory}/.authinfo";
}
