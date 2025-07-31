{ pkgs, config, ... }:
let
  rss-to-kindle-source = builtins.readFile (pkgs.replaceVars ./rss-to-kindle-generate.sh {
    bash = "${pkgs.bash}/bin/bash";
    kindle-send = "${config.my.kindle-send}/bin/kindle-send";
    ebook-convert = "${pkgs.calibre}/bin/ebook-convert";
  });

  rss-to-kindle-generate = pkgs.writeShellScriptBin "rss-to-kindle-generate" rss-to-kindle-source;
in
{
  environment.systemPackages = [
    pkgs.calibre
    pkgs.epubcheck
    rss-to-kindle-generate
  ];

  environment.etc."freshrss.recipe".text = builtins.readFile ./freshrss-recipe.py;

  systemd.timers.rss-to-kindle-generate = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "03:00";
      Persistent = true;
      AccuracySec = "1h";
      Unit = "rss-to-kindle-generate.service";
    };
  };

  systemd.services.rss-to-kindle-failure-notify = {
    description = "Send error notification if rss-to-kindle-generate fails";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        /bin/sh -c '
          subject="RSS-to-Kindle FAILED on $(hostname) at $(date)"
          body=$(journalctl -u rss-to-kindle-generate --since "1 hour ago")
          echo "$body" | mutt -F /etc/Muttrc -s "$subject" -- sendtokindleerrors@addisonbeck.com
        '
      '';
    };
  };

  systemd.services.rss-to-kindle-generate = {
    description = "Generate and send FreshRSS digest to Kindle";
    script = "/run/current-system/sw/bin/rss-to-kindle-generate";
    serviceConfig = {
      User = "root";
      Type = "oneshot";
    };
    onFailure = [ "rss-to-kindle-failure-notify.service" ];
  };
}
