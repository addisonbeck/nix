#!/usr/bin/env bash
set -euo pipefail

NODE_REF="${1:-}"
if [[ -z "${NODE_REF}" ]]; then
  echo "Usage: memory-to-epub-file <ROAM_ID|/absolute/path/to/file.org|file:/absolute/path>" >&2
  exit 1
fi

OUTPUT_DIR="$HOME/notes/epubs"
mkdir -p "$OUTPUT_DIR"
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

RAW="$NODE_REF"
if [[ "$RAW" == file:* ]]; then
  RAW="${RAW#file:}"
fi

if [[ "$RAW" == /* ]] && [[ -f "$RAW" ]]; then
  ORG_FILE="$RAW"
else
  ORG_FILE="$(@python3@ @org-roam-find-node-file@ "$RAW" 2>/dev/null || true)"
fi

if [[ -z "${ORG_FILE:-}" || ! -f "${ORG_FILE}" ]]; then
  echo "Error: Could not resolve Org file from '$NODE_REF'" >&2
  exit 2
fi

TITLE=$(grep -m1 '^#+TITLE:' "$ORG_FILE" | sed -E 's/^#\+TITLE:[[:space:]]*//')
if [[ -z "$TITLE" ]]; then
  TITLE="$(basename "$ORG_FILE")"
fi

slugify() { printf '%s' "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g; s/^-+|-+$//g'; }
SLUG="$(slugify "$TITLE")"

METADATA_FILE="$TMPDIR/metadata.yaml"
printf -- '---\ntitle: "%s"\n...\n' "${TITLE//\"/\\\"}" > "$METADATA_FILE"

EPUB="$OUTPUT_DIR/${SLUG}.epub"

# Fetch a random APOD image for the cover (skipped if no token is configured)
COVER=""
if [[ -n "@nasa-token-path@" ]]; then
NASA_API_KEY="$(cat @nasa-token-path@)"
for _attempt in 1 2 3; do
  APOD_JSON=$(@curl@ -sf "https://api.nasa.gov/planetary/apod?api_key=${NASA_API_KEY}&count=1" || true)
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
fi

if @pandoc@ "$ORG_FILE" -o "$EPUB" \
  --from=org \
  --to=epub3 \
  --toc \
  --toc-depth=2 \
  --split-level=1 \
  --metadata-file "$METADATA_FILE" \
  --metadata lang="en" \
  --standalone \
  --highlight-style=pygments \
  -L @mermaid-lua-filter@ \
  --resource-path "$(dirname "$ORG_FILE")" \
  --epub-embed-font="@iosevka-regular@" \
  --epub-embed-font="@iosevka-bold@" \
  --epub-embed-font="@iosevka-italic@" \
  ${COVER:+--epub-cover-image="$COVER"} \
  --css=@memory-css@; then
  echo "EPUB written to: $EPUB"
else
  if grep -q '#+BEGIN_SRC mermaid\|```mermaid' "$ORG_FILE" 2>/dev/null; then
    echo "Error: pandoc failed — the file contains mermaid blocks; check diagram syntax" >&2
  else
    echo "Error: pandoc failed" >&2
  fi
  exit 1
fi
