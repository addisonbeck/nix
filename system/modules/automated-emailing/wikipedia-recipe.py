# vim:fileencoding=UTF-8:ts=4:sw=4:sta:et:sts=4:ai
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os
from collections import Counter

from calibre.ebooks.oeb.base import XPath, xml2text
from calibre.ptempfile import PersistentTemporaryFile
from calibre.utils.localization import canonicalize_lang
from calibre.web.feeds.news import BasicNewsRecipe
from polyglot.urllib import unquote, urldefrag, urlparse

DEFAULT_TITLE = 'Wiki Article'
LOGO = None


def populate_toc(parent, ul):
    for a in XPath('./h:li/h:a[starts-with(@href, "#")]')(ul):
        child = parent.add(xml2text(a), urldefrag(parent.href)[0] +
                           a.get('href'))
        grandchild = XPath('./h:ul')(a.getparent())
        if grandchild:
            populate_toc(child, grandchild[0])


class WikiPage(BasicNewsRecipe):
    title = DEFAULT_TITLE
    language = 'en'
    __author__ = 'Krittika Goyal'
    oldest_article = 1  # days
    max_articles_per_feed = 99999999
    use_embedded_content = False
    resolve_internal_links = True

    no_stylesheets = True
    extra_css = """body{font-family: Helvetica,Arial,sans-serif } """
    conversion_options = {
        'authors': 'Wikipedia',
        'tags': 'Wikipedia',
        'publisher': 'Wikipedia',
        'epub_flatten': True,
    }

    publication_type = ''
    keep_only_tags = [dict(name='h1', attrs={'id': ['firstHeading']}), dict(
        name='div', attrs={'id': ['bodyContent']})]

    remove_tags = [
        dict(name='span', attrs={'class': ['mw-editsection', 'editsection']}),
        dict(name='div', attrs={'id': ['catlinks', 'mw-articlefeedbackv5']}),
        dict(name='table', attrs={'class': 'navbox'}),
        #dict(name='table', attrs={'class': 'infobox'}),
        dict(name='table', attrs={'class': 'metadata plainlinks mbox-small'}),
        dict(name='table', attrs={'class': 'metadata mbox-small plainlinks'}),
    ]

    def parse_book_page(self, url):
        purl = urlparse(url)
        base = purl.scheme + '://' + purl.netloc
        root = self.index_to_soup(url, as_tree=True)
        div = root.xpath('//div[@id="mw-content-text"]')[0]
        heading = root.xpath('//h2')[0]
        try:
            self.title = self.tag_to_string(heading.xpath('./span')[0])
        except IndexError:
            self.title = None
        self.timefmt = ''
        current_section, current_articles = 'Articles', []
        feeds = []
        for dl in div.xpath('./dl'):
            for x in dl.xpath('./*[self::dt|self::dd]'):
                if x.tag == 'dt':
                    if current_section and current_articles:
                        feeds.append((current_section, current_articles))
                    current_section = self.tag_to_string(x)
                    if not self.title:
                        self.title = current_section
                    current_articles = []
                    self.log('Section:', current_section)
                else:
                    a = x.xpath('descendant::a[@href]')
                    if a:
                        a = a[0]
                        title = self.tag_to_string(a)
                        url = a.get('href')
                        if url.startswith('/'):
                            url = base + url
                        current_articles.append({'title': title, 'url': url})
                        self.log('\tArticle:', title, ':', url)
        if current_section and current_articles:
            feeds.append((current_section, current_articles))
        return feeds

    def parse_index(self):
        urls = []  # REPLACE_ME_URLS
        self.seen_languages = Counter()
        if not urls:
            # Testing
            urls = [
                'https://en.wikipedia.org/wiki/Jane_Austen',
                'https://en.wikipedia.org/wiki/Pride_and_Prejudice',
                # 'https://en.wikipedia.org/wiki/Book:3D_printing',
            ]
        if isinstance(urls, (bytes, type(u''))):
            return self.parse_book_page(urls)
        current_articles = []
        feeds = []
        for url in urls:
            segments = url.split('/')
            titlelong = unquote(segments[-1])
            title = titlelong.replace('_', ' ')
            self.log('\t\tFound article:', title)
            self.log('\t\t\t', url)
            current_articles.append({'title': title, 'url': url,
                                     'description': '', 'date': ''}),

        if current_articles:
            feeds.append(('Articles', current_articles))

        return feeds

    def preprocess_html(self, soup):
        # Fix relative links
        link = soup.find('link', rel='canonical', href=True)
        url_prefix = None
        if link is not None:
            pu = urlparse(link['href'])
            url_prefix = pu.scheme + '://' + pu.hostname
        for a in soup.findAll(
                ['a', 'area', 'link'],
                href=lambda x: x and x.startswith('/')):
            href = a['href']
            if href.startswith('//'):
                a['href'] = 'https:' + href
            elif url_prefix:
                a['href'] = url_prefix + a['href']
        # Improve formatting of inner images
        for div in soup.findAll('div', attrs={
                'class': lambda x: x and 'thumbinner' in x.split()}):
            div['style'] = ('background-color:#eee; border: solid 1px gray;'
                            ' text-align:center')
        return soup

    def populate_article_metadata(self, article, soup, first):
        if first:
            title = soup.find('title')
            if title:
                title = self.tag_to_string(title)
                pos = title.find(' - ')
                if pos < 0:
                    pos = title.find(' – ')
                if pos > 0:
                    title = title[:pos]
                article.title = title
            try:
                language = soup.find('html')['lang']
                if language:
                    lcode = canonicalize_lang(language)
                    if lcode:
                        self.seen_languages[lcode] += 1
            except Exception:
                self.log.error('Failed to detect language for article', title)

    def create_opf(self, feeds, dir=None):
        'Set the title of the first article as the title of this book'
        if self.title == DEFAULT_TITLE:
            for f in feeds:
                for a in f:
                    self.title = a.title
                    break
        langs = self.seen_languages.most_common()
        if langs:
            self.language = langs[0][0]
        return BasicNewsRecipe.create_opf(self, feeds, dir=dir)

    def default_cover(self, cover_file):
        'Use the wikipedia logo on the cover instead of the calibre logo'
        if LOGO is None:
            return False  # Testing
        lf = PersistentTemporaryFile('_logo.png')
        with open(LOGO, 'rb') as f:
            lf.write(f.read())
        lf.close()

        try:
            from calibre.ebooks import calibre_cover
            img_data = calibre_cover(
                self.title, 'Wikipedia', logo_path=lf.name)
            cover_file.write(img_data)
            cover_file.flush()
            os.remove(lf.name)
        except Exception:
            self.log.exception('Failed to generate default cover')
            return False
        return True

    def postprocess_book(self, oeb, opts, log):
        # Remove the useless first page, it contains the list of sections
        oeb.manifest.remove(oeb.spine[0])
        # Populate the Table of Contents with the entries from the ToC of the
        # article
        for entry in oeb.toc:
            try:
                item = oeb.manifest.hrefs[urldefrag(entry.href)[0]]
            except KeyError:
                continue
            toc = XPath('//h:table[@id="toc"]/h:tr/h:td/h:ul')(item.data)
            if not toc:
                continue
            toc = toc[0]
            populate_toc(entry, toc)
