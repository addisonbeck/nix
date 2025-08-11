#!/usr/bin/env bash
set -euo pipefail

RESOLVER="@resolver@"
PANDOC="@pandoc@"
KINDLE_SEND="@kindle-send@"
BASH="@bash@"

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
  ORG_FILE="$("$RESOLVER" --id "$ID")"
else
  ORG_FILE="$("$RESOLVER" --file "$FILE")"
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
  # crude slug for filenames
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

PANDOC_ARGS=( "$ORG_FILE" -o "$EPUB" --toc --toc-depth=3 --standalone --metadata "title=$TITLE" )
if [[ -n "$CSS" ]]; then
  PANDOC_ARGS+=( --css "$CSS" )
fi

"$PANDOC" "${PANDOC_ARGS[@]}"

if [[ -z "$SUBJECT" ]]; then
  SUBJECT="Org: ${TITLE} (${DATE})"
fi

"$BASH" "$KINDLE_SEND" "$EPUB" "$ZIP" "$SUBJECT" "$TMPDIR"

echo "Sent '$TITLE' to Kindle (subject: $SUBJECT)"
