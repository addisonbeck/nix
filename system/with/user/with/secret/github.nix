{
  config,
  pkgs,
  ...
}: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = ["${config.home.homeDirectory}/.ssh/me"];
  age.secrets.github.file = ./github.age;
  # SSH package defaults to the one provided by the system. Setting this
  # explicitly to pkgs pulls it from nixpkgs even on Darwin
}
