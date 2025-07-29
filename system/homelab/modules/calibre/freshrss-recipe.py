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
        ('Wikipedia News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=5Xf0A2fxDBPkGMc1CQ7m6h&f=rss')
        #('Hacker News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2tt7F0YwadNyfZQfh5FZvv&f=rss'),
        #('AP News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2n9Bmn0vxtWgJx6Y8PzjOi&f=rss'),
        #('Bitwarden News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=57L3a8oIcNzNQLmnFkuF4S&f=rss')
        #('Local News', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2ypxFazPTg88WyAzuztQRN&f=rss')
        #('Computer Science Blogs', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=DFnbwQpir2UzYaZ9v00cO&f=rss'),
        #('Misc', 'https://homelab.tail357e32.ts.net/rss/api/query.php?user=me&t=2ESHVeebkckacH24XNDxIv&f=rss')
    ]
