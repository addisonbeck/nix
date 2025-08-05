#!/usr/bin/env bash
set -euo pipefail

ORGROAM_SCRIPT="@org-roam-find-file@"
ORG_FILE=$("$ORGROAM_SCRIPT")

if [ -z "$ORG_FILE" ]; then
  echo "Error: Could not find org file for node ID $NODEID" >&2
  exit 1
fi

mkdir -p /var/lib/cookbook-to-kindle
DATE=$(TZ=America/New_York date +%F-%H-%M-%S)
EPUB="/var/lib/cookbook-to-kindle/cookbook.epub"
ZIP="/var/lib/cookbook-to-kindle/cookbook.zip"
TMPDIR="/var/lib/cookbook-to-kindle/tmp_${DATE}"
mkdir -p "$TMPDIR"

@pandoc@ "$ORG_FILE" -o "$EPUB" \
       --toc \
       --toc-depth=3 \
       --split-level=2 \
       --metadata title="Addison's Cookbook" \
       --standalone \
       --css=@cookbook-css@

# Send to Kindle
SUBJECT="Cookbook for Kindle ($DATE)"
@bash@ @kindle-send@ "$EPUB" "$ZIP" "$SUBJECT" "$TMPDIR"
