{
  config,
  pkgs,
  ...
} :
{
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = [ "${config.home.homeDirectory}/.ssh/me" ];
  age.secrets.github.file = ./github.age;

  programs.git.signing.key = config.age.secrets.github.path;
  programs.git.extraConfig.gpg.format = "ssh";
  programs.git.signing.signByDefault = true;
  programs.git.userName = "Addison Beck";
  programs.git.userEmail = "github@addisonbeck.com";
  programs.git.extraConfig.pull.rebase = true;

  programs.ssh.enable = true;

  # SSH package defaults to the one provided by the system. Setting this
  # explicitly to pkgs pulls it from nixpkgs even on Darwin
  programs.ssh.package = pkgs.openssh;

  programs.ssh.matchBlocks = {
    "github.com" = {
      hostname = "github.com";
      identityFile = config.age.secrets.github.path;
    };
  };
}
