{ lib, pkgs, ... }: {
  home.packages = [
    (pkgs.weechat.override {
      configure = { availablePlugins, ... }: {
        plugins = with availablePlugins; [
          (perl.withPackages (p: [ p.PodParser ]))
          (python.withPackages (ps:
            [
              ps.websocket_client
              # ps.pync # requires 2.x
            ]))
        ];
        scripts = with pkgs.weechatScripts;
          [ wee-slack weechat-autosort colorize_nicks ]
          ++ lib.optionals (!pkgs.stdenv.isDarwin) [ weechat-notify-send ];
      };
    })
  ];
}
