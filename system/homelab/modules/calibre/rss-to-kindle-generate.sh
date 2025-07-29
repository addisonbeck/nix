#!/usr/bin/env bash
set -euo pipefail
mkdir -p /var/lib/rss-to-kindle
DATE=$(TZ=America/New_York date +%F-%H-%M-%S)
EPUB="/var/lib/rss-to-kindle/freshrss_${DATE}.epub"
ZIP="/var/lib/rss-to-kindle/freshrss_${DATE}.zip"
TMPDIR="/var/lib/rss-to-kindle/tmp_${DATE}"
mkdir -p "$TMPDIR"
@ebook-convert@ /etc/freshrss.recipe "$EPUB"
SUBJECT="FreshRSS Digest for ${DATE}"
@bash@ @kindle-send@ "$EPUB" "$ZIP" "$SUBJECT" "$TMPDIR"
