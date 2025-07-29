{ pkgs, config, ... }:
let
  wikiReaderPlugin = pkgs.runCommand "wiki-reader-calibre-plugin" {
    pluginZip = ./Wiki_Reader.zip;
  } ''
    mkdir -p $out
    cp $pluginZip $out/Wiki_Reader.zip
  '';

  kindle-send-source = builtins.readFile (pkgs.replaceVars ./kindle-send.sh {
      mutt = "${pkgs.mutt}/bin/mutt";
      zip = "${pkgs.zip}/bin/zip";
      # muttrc is configured in homelab/modules/mutt
      # maybe I should really have a specific rc for these operations
      muttrc = "${config.environment.etc."Muttrc".source}";
      sendTo = "us+amazon_WGMxDE@kindle.com";
      epubcheck = "${pkgs.epubcheck}/bin/epubcheck";
  });
  kindle-send = pkgs.writeShellScriptBin "kindle-send" kindle-send-source;

  rss-to-kindle-source = builtins.readFile (pkgs.replaceVars ./rss-to-kindle-generate.sh {
    bash = "${pkgs.bash}/bin/bash";
    kindle-send = "${kindle-send}/bin/kindle-send";
    ebook-convert = "${pkgs.calibre}/bin/ebook-convert";
  });

  rss-to-kindle-generate = pkgs.writeShellScriptBin "rss-to-kindle-generate" rss-to-kindle-source;
  wikipedia-to-kindle-source = builtins.readFile (pkgs.replaceVars ./wikipedia-to-kindle-generate.sh {
    bash = "${pkgs.bash}/bin/bash";
    kindle-send = "${kindle-send}/bin/kindle-send";
    ebook-convert = "${pkgs.calibre}/bin/ebook-convert";
  });
  wikipedia-to-kindle-generate = pkgs.writeShellScriptBin "wikipedia-to-kindle-generate" wikipedia-to-kindle-source;
in
{
  environment.systemPackages = [
    pkgs.calibre
    pkgs.epubcheck
    pkgs.zip
    kindle-send
    rss-to-kindle-generate
    wikipedia-to-kindle-generate
  ];

  environment.etc."freshrss.recipe".text = builtins.readFile ./freshrss-recipe.py;
  environment.etc."wikipedia.recipe".text = builtins.readFile ./wikipedia-recipe.py;

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
