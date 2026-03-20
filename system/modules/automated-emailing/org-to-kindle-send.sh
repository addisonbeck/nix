#!/usr/bin/env bash
set -euo pipefail

PYTHON3="@python3@"
ORG_ROAM_FIND_NODE_FILE="@org-roam-find-node-file@"
PANDOC="@pandoc@"
KINDLE_SEND="@kindle-send@"
BASH="@bash@"
MEMORY_CSS="@memory-css@"

usage() {
  cat <<EOF
Usage: org-to-kindle-send (--id ROAM_ID | --file PATH) [--title TITLE] [--subject SUBJECT] [--css CSS]
Sends the specified Org file to Kindle as an EPUB via email.
EOF
}

ID=""
FILE=""
TITLE=""
SUBJECT=""
CSS=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --id) ID="$2"; shift 2;;
    --file) FILE="$2"; shift 2;;
    --title) TITLE="$2"; shift 2;;
    --subject) SUBJECT="$2"; shift 2;;
    --css) CSS="$2"; shift 2;;
    -h|--help) usage; exit 0;;
    *) echo "Unknown arg: $1" >&2; usage; exit 1;;
  esac
done

if [[ -z "$ID" && -z "$FILE" ]]; then
  echo "Error: Provide --id or --file" >&2
  usage; exit 1
fi

# Resolve the org file path
if [[ -n "$ID" ]]; then
  ORG_FILE="$("$PYTHON3" "$ORG_ROAM_FIND_NODE_FILE" "$ID" 2>/dev/null)"
else
  RAW="$FILE"
  if [[ "$RAW" == file:* ]]; then RAW="${RAW#file:}"; fi
  ORG_FILE="$RAW"
fi

if [[ -z "${ORG_FILE:-}" || ! -f "$ORG_FILE" ]]; then
  echo "Error: Could not resolve org file" >&2
  exit 1
fi

derive_title() {
  local f="$1"
  local t
  t="$(grep -m1 -E '^#\+TITLE:' "$f" | sed -E 's/^#\+TITLE:\s*//')"
  if [[ -z "$t" ]]; then
    t="$(basename "$f" .org)"
  fi
  echo "$t"
}

safe_slug() {
  echo "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g;s/^-+|-+$//g'
}

OUTDIR="/var/lib/org-to-kindle"
mkdir -p "$OUTDIR"
DATE="$(TZ=America/New_York date +%F-%H-%M-%S)"

if [[ -z "$TITLE" ]]; then
  TITLE="$(derive_title "$ORG_FILE")"
fi
SLUG="$(safe_slug "$TITLE")"
EPUB="$OUTDIR/${SLUG}.epub"
ZIP="$OUTDIR/${SLUG}.zip"
TMPDIR="$OUTDIR/tmp_${DATE}"
mkdir -p "$TMPDIR"

METADATA_FILE="$TMPDIR/metadata.yaml"
printf -- '---\ntitle: "%s"\n...\n' "${TITLE//\"/\\\"}" > "$METADATA_FILE"

CSS="${CSS:-$MEMORY_CSS}"

"$PANDOC" "$ORG_FILE" -o "$EPUB" \
  --from=org \
  --to=epub3 \
  --toc \
  --toc-depth=3 \
  --standalone \
  --highlight-style=monochrome \
  --metadata-file "$METADATA_FILE" \
  --css="$CSS"

if [[ -z "$SUBJECT" ]]; then
  SUBJECT="Org: ${TITLE} (${DATE})"
fi

"$BASH" "$KINDLE_SEND" "$EPUB" "$ZIP" "$SUBJECT" "$TMPDIR"

echo "Sent '$TITLE' to Kindle (subject: $SUBJECT)"
