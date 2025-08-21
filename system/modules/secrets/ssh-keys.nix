{pkgs, config, ...}: {
  sops.secrets = {
    "known_hosts" = {
      format = "binary";
      sopsFile = ../../../secrets/known_hosts;
      path = "${config.home.homeDirectory}/.ssh/known_hosts";
      key = "primary_yk/public";
      mode = "0400";
    };

    "primary-ssh-key.pub" = {
      format = "yaml";
      sopsFile = ../../../secrets/ssh-keys.yaml;
      path = "${config.home.homeDirectory}/.ssh/primary-ssh-key.pub";
      key = "primary_yk/public";
      mode = "0400";
    };

    "primary-ssh-key" = {
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
    #testing = {
      #format = "yaml";
      #sopsFile = ../../../secrets/testing.yaml;
    #};
  };

  programs.ssh = {
    enable = true;
    package = pkgs.openssh;
    extraConfig = ''
      AddKeysToAgent yes
      IgnoreUnknown UseKeychain
      UseKeychain yes
    '';
    userKnownHostsFile = config.sops.secrets."known_hosts".path;

    # Don't forget to run ssh-add -K on new client machines
    matchBlocks = {
      "github.com" = {
        hostname = "github.com";
        identityFile = config.sops.secrets."primary-ssh-key".path;
      };
      "homelab" = {
        hostname = "homelab";
        user = "root";
        identityFile = config.sops.secrets."primary-ssh-key".path;
        forwardAgent = true;
      };
      "addisonbeck.com" = {
        hostname = "addisonbeck.com";
        user = "root";
        identityFile = config.sops.secrets."primary-ssh-key".path;
        forwardAgent = false;
      };
    };
  };

  programs.git = {
    userName = "addisonbeck";
    userEmail = "github@addisonbeck.com";
    signing.key = config.sops.secrets."primary-ssh-key.pub".path;
    extraConfig.gpg.format = "ssh";
    signing.signByDefault = true;
  };
}
