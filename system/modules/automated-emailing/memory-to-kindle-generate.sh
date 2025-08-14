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

@pandoc@ "$ORG_FILE" -o "$EPUB" \
  --from=org \
  --to=epub3 \
  --toc \
  --toc-depth=2 \
  --epub-chapter-level=1 \
  --metadata title="$TITLE" \
  --standalone \
  --css=@memory-css@

SUBJECT="Memory: $TITLE ($DATE)"
@bash@ @kindle-send@ "$EPUB" "$ZIP" "$SUBJECT" "$TMPDIR"
