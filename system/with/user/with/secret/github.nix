{
  config,
  ...
} :
{
  age.secrets.github.file = ./github.age;
  age.secretsDir = "/home/me/.secrets";
  programs.ssh.matchBlocks = {
    "github.com" = {
      hostname = "github.com";
      identityFile = config.age.secrets.github.path;
    };
  };
}
