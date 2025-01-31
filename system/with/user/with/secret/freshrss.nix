{
  config,
  ...
}: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/me"];
  age.secrets.freshrss.file = ./freshrss.age;
}
