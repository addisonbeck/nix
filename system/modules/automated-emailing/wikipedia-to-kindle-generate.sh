#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: wikipedia-to-kindle-generate <wikipedia-article-url>" >&2
  exit 2
fi

URL="$1"
TITLE=$(echo "$URL" | sed -E 's#.*/wiki/##')
TITLE_SAFE=$(echo "$TITLE" | tr -c '[:alnum:]' '_')

mkdir -p /var/lib/wikipedia-to-kindle
DATE=$(TZ=America/New_York date +%F-%H-%M-%S)
EPUB="/var/lib/wikipedia-to-kindle/${TITLE_SAFE}_${DATE}.epub"
ZIP="/var/lib/wikipedia-to-kindle/${TITLE_SAFE}_${DATE}.zip"
TMPDIR="/var/lib/wikipedia-to-kindle/tmp_${DATE}"
mkdir -p "$TMPDIR"

# Run the plugin to generate an EPUB

sed "s~urls = \[\]  # REPLACE_ME_URLS~urls = [\"$URL\"]~" /etc/wikipedia.recipe > "$TMPDIR/wikipedia.recipe"
@ebook-convert@ "$TMPDIR/wikipedia.recipe" "$EPUB"

# Send to Kindle
SUBJECT="Wikipedia Article for Kindle ($DATE)"
@bash@ @kindle-send@ "$EPUB" "$ZIP" "$SUBJECT" "$TMPDIR"
