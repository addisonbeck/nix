{...}: {
  programs.newsboat.enable = true;
  programs.newsboat.autoReload = true;
  programs.newsboat.browser = "open";
  programs.newsboat.extraConfig = ''
    show-read-feeds yes

    bind-key j down feedlist
    bind-key k up feedlist
    bind-key j next articlelist
    bind-key k prev articlelist
    bind-key J next-feed articlelist
    bind-key K prev-feed articlelist
    bind-key j down article
    bind-key k up article

    unbind-key C feedlist
    confirm-exit no
  '';
  programs.newsboat.urls = [
    {
      title = "HN";
      url = "https://news.ycombinator.com/rss";
      tags = ["hn"];
    }
    {url = "https://lukesmith.xyz/rss.xml";}
    {url = "https://www.hanselman.com/blog/";}
    {url = "https://github.com/bitwarden/server/commits.atom";}
  ];
}
