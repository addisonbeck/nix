#!/usr/bin/env bash
set -euo pipefail

NODE_REF="${1:-}"
if [[ -z "${NODE_REF}" ]]; then
  echo "Usage: memory-to-kindle-generate <ROAM_ID|/absolute/path/to/file.org|file:/absolute/path>" >&2
  exit 1
fi

mkdir -p /var/lib/memory-to-kindle
DATE=$(TZ=America/New_York date +%F-%H-%M-%S)
TMPDIR="/var/lib/memory-to-kindle/tmp_${DATE}"
mkdir -p "$TMPDIR"

RAW="$NODE_REF"
# FIX: handle any file: prefix (file:/… or file:///…)
if [[ "$RAW" == file:* ]]; then
  RAW="${RAW#file:}"
fi

# FIX: detect absolute paths correctly (any string starting with /)
if [[ "$RAW" == /* ]] && [[ -f "$RAW" ]]; then
  ORG_FILE="$RAW"
else
  # Treat as Roam ID (allow id:ID or bare ID)
  ORG_FILE="$(@python3@ @org-roam-find-node-file@ "$RAW" 2>/dev/null || true)"
fi

if [[ -z "${ORG_FILE:-}" || ! -f "${ORG_FILE}" ]]; then
  echo "Error: Could not resolve Org file from '$NODE_REF'" >&2
  exit 2
fi

TITLE=$(awk -F':' '/^#\+TITLE:/ { sub(/^[ \t]+/,"",$2); print $2; exit }' "$ORG_FILE")
if [[ -z "$TITLE" ]]; then
  TITLE="$(basename "$ORG_FILE")"
fi


slugify() { printf '%s' "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g; s/^-+|-+$//g'; }
SLUG="$(slugify "$TITLE")"

EPUB="/var/lib/memory-to-kindle/${SLUG}.epub"
ZIP="/var/lib/memory-to-kindle/${SLUG}.zip"

# Fetch a random APOD image for the cover
COVER=""
for _attempt in 1 2 3; do
  APOD_JSON=$(@curl@ -sf "https://api.nasa.gov/planetary/apod?api_key=DEMO_KEY&count=1" || true)
  if [[ -z "$APOD_JSON" ]]; then break; fi
  APOD_TYPE=$(@python3@ -c "import sys,json; d=json.loads(sys.stdin.read())[0]; print(d.get('media_type',''))" <<< "$APOD_JSON")
  if [[ "$APOD_TYPE" == "image" ]]; then
    APOD_URL=$(@python3@ -c "import sys,json; d=json.loads(sys.stdin.read())[0]; print(d.get('hdurl') or d.get('url',''))" <<< "$APOD_JSON")
    COVER_RAW="$TMPDIR/cover_raw.jpg"
    COVER="$TMPDIR/cover.jpg"
    if @curl@ -sf -L "$APOD_URL" -o "$COVER_RAW"; then
      @magick@ "$COVER_RAW" -resize 1600x2560^ -gravity center -extent 1600x2560 "$COVER" || COVER=""
    else
      COVER=""
    fi
    break
  fi
done

@pandoc@ "$ORG_FILE" -o "$EPUB" \
  --from=org \
  --to=epub3 \
  --toc \
  --toc-depth=2 \
  --split-level=1 \
  --metadata title="$TITLE" \
  --metadata lang="en" \
  --standalone \
  --highlight-style=monochrome \
  ${COVER:+--epub-cover-image="$COVER"} \
  --epub-embed-font="@iosevka-etoile-fonts@/IosevkaEtoile-Regular.ttc" \
  --epub-embed-font="@iosevka-etoile-fonts@/IosevkaEtoile-Bold.ttc" \
  --epub-embed-font="@iosevka-code-fonts@/Iosevka-Regular.ttc" \
  --epub-embed-font="@iosevka-code-fonts@/Iosevka-Bold.ttc" \
  --css=@memory-css@

SUBJECT="Memory: $TITLE ($DATE)"
@bash@ @kindle-send@ "$EPUB" "$ZIP" "$SUBJECT" "$TMPDIR"
