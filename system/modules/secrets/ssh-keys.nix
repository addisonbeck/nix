{pkgs, config, ...}: {
  programs.ssh.enable = true;
  programs.ssh.package = pkgs.openssh;

  sops.secrets."primary-ssh-key.pub" = {
    format = "yaml";
    sopsFile = ../../../secrets/ssh-keys.yaml;
    path = "${config.home.homeDirectory}/.ssh/primary-ssh-key.pub";
    key = "primary_yk/public";
    mode = "0400";
  };

  sops.secrets."primary-ssh-key" = {
    format = "yaml";
    sopsFile = ../../../secrets/ssh-keys.yaml;
    path = "${config.home.homeDirectory}/.ssh/primary-ssh-key";
    key = "primary_yk/private";
    mode = "0400";
  };

  # This key is only decryptable by a hardware backed gpg key. It would be
  # _so cool_ to be able to use keys like that during system activation, but
  # I haven't been able to get that to work. Leaving for now as a reminder to
  # my future self to try again during my next key cycle & system iteration.
  #sops.secrets.testing = {
    #format = "yaml";
    #sopsFile = ../../../secrets/testing.yaml;
  #};

  programs.git.signing.key = config.sops.secrets."primary-ssh-key.pub".path;
  programs.git.extraConfig.gpg.format = "ssh";
  programs.git.signing.signByDefault = true;
  programs.git.userName = "addisonbeck";
  programs.git.userEmail = "github@addisonbeck.com";

  programs.ssh.matchBlocks = {
    "github.com" = {
      hostname = "github.com";
      identityFile = config.sops.secrets."primary-ssh-key".path;
    };
  };

  programs.ssh.matchBlocks = {
    "homelab" = {
      hostname = "homelab";
      identityFile = config.sops.secrets."primary-ssh-key".path;
    };
  };
}
