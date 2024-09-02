{ config, ... }: {
  age.secretsDir = "${config.home.homeDirectory}/.secrets";
  age.identityPaths = [ "${config.home.homeDirectory}/.ssh/me" ];
  age.secrets.weechat-plugins-config.file = ./weechat-plugins-config.age;
  age.secrets.weechat-plugins-config.path = "${config.home.homeDirectory}/.config/weechat/plugins.conf";
}
