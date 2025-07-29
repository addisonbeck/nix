{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.calibre
    pkgs.zip
    (pkgs.writeShellScriptBin "rss-to-kindle-generate" ''
      set -euo pipefail
      mkdir -p /var/lib/rss-to-kindle
      DATE=$(TZ=America/New_York date +%F-%H-%M-%S)
      ebook-convert /etc/freshrss.recipe "/var/lib/rss-to-kindle/freshrss_''${DATE}.epub" -vv
      echo "FreshRSS Digest for ''${DATE}" | mutt -F /etc/Muttrc -a "/var/lib/rss-to-kindle/freshrss_''${DATE}.epub" -s "FreshRSS Digest" -- us+amazon_WGMxDE@kindle.com
      rm /var/lib/rss-to-kindle/freshrss_''${DATE}.epub
    '')
    (pkgs.writeShellScriptBin "wikipedia-rss-to-kindle-generate" ''
      set -euo pipefail
      mkdir -p /var/lib/rss-to-kindle
      DATE=$(TZ=America/New_York date +%F-%H-%M-%S)
      ebook-convert /etc/wikipedia-freshrss.recipe "/var/lib/rss-to-kindle/wikipedia-freshrss_''${DATE}.epub" -vv
      echo "FreshRSS Digest for ''${DATE}" | mutt -F /etc/Muttrc -a "/var/lib/rss-to-kindle/wikipedia-freshrss_''${DATE}.epub" -s "Wikipedia Digest" -- us+amazon_WGMxDE@kindle.com
      rm /var/lib/rss-to-kindle/wikipedia-freshrss_''${DATE}.epub
    '')
    (pkgs.writeShellScriptBin "wikipedia-to-kindle" ''
      set -euo pipefail
      if [ "$#" -ne 1 ]; then
        echo "Usage: wikipedia-to-kindle <wikipedia-article-url>" >&2
        exit 2
      fi
      URL="$1"
      mkdir -p /var/lib/wikipedia-to-kindle
      DATE=$(TZ=America/New_York date +%F-%H-%M-%S)
      EPUB="/var/lib/wikipedia-to-kindle/article_''${DATE}.epub"

      # Download Wikipedia article HTML
      TMPHTML="/var/lib/wikipedia-to-kindle/article_''${DATE}.html"
      curl -L "$URL" -o "$TMPHTML"

      # Convert HTML to EPUB (preserves images, tables, refs)
      #ebook-convert "$TMPHTML" "$EPUB" --enable-heuristics --pretty-print --chapter --no-default-epub-cover
      ebook-convert "$TMPHTML" "/var/lib/wikipedia-to-kindle/article_''${DATE}.epub" -vv

      SUBJECT="Wikipedia Article for Kindle ($DATE)"
      echo "Wikipedia article sent to Kindle." | mutt -F /etc/Muttrc -a "/var/lib/wikipedia-to-kindle/article_''${DATE}.epub" -s "$SUBJECT" -- us+amazon_WGMxDE@kindle.com
    '')
  ];

  environment.etc."freshrss.recipe".text = ''
    from calibre.web.feeds.news import BasicNewsRecipe
    class FreshRSSRecipe(BasicNewsRecipe):
        title = 'FreshRSS Digest'
        oldest_article = 10
        max_articles_per_feed = 50
        auto_cleanup = True
        ignore_duplicate_articles = {'title', 'url'}
        remove_tags = [dict(name='iframe'), dict(name='video')]
        browser_type = 'qt'
        conversion_options = {
          'base_font_size': 16
        }
        feeds = [
            ('Hacker News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2tt7F0YwadNyfZQfh5FZvv&f=rss'),
            ('AP News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2n9Bmn0vxtWgJx6Y8PzjOi&f=rss'),
            ('Local News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2ypxFazPTg88WyAzuztQRN&f=rss')
        ]
  '';
            #('Bitwarden News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=57L3a8oIcNzNQLmnFkuF4S&f=rss'),
            #('Computer Science Blogs', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=DFnbwQpir2UzYaZ9v00cO&f=rss'),
            #('Misc', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2ESHVeebkckacH24XNDxIv&f=rss')

  environment.etc."wikipedia-freshrss.recipe".text = ''
    from calibre.web.feeds.news import BasicNewsRecipe
    class FreshRSSRecipe(BasicNewsRecipe):
        title = 'Wikipedia Featured Articles And News'
        oldest_article = 10
        max_articles_per_feed = 50
        auto_cleanup = True
        ignore_duplicate_articles = {'title', 'url'}
        remove_tags = [dict(name='iframe'), dict(name='video')]
        browser_type = 'qt'
        conversion_options = {
          'base_font_size': 16
        }
        feeds = [
            ('Wikipedia News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=5Xf0A2fxDBPkGMc1CQ7m6h&f=rss')
        ]
  '';

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
