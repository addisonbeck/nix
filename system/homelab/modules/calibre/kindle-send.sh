#!/usr/bin/env bash
set -euo pipefail
EPUB="$1"
ZIP="$2"
SUBJECT="$3"
TMPDIR="$4"

#@epubcheck@ "$EPUB"
@zip@ -j "$ZIP" "$EPUB"
MAX_SIZE=52428800
ACTUAL_SIZE=$(stat -c%s "$ZIP")
if [ "$ACTUAL_SIZE" -gt "$MAX_SIZE" ]; then
  echo "ERROR: $ZIP is too large ($ACTUAL_SIZE bytes)." >&2
  rm "$EPUB" "$ZIP"
  exit 1
fi
echo "$SUBJECT" | @mutt@ -F @muttrc@ -a "$ZIP" -s "$SUBJECT" -- @sendTo@
rm "$EPUB" "$ZIP"
rm -rf "$TMPDIR"
