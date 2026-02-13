#!/usr/bin/env bash
# ~/.claude/skills/create_memory/create_memory.sh

set -euo pipefail

# Parse JSON input from stdin
input=$(cat)
title=$(echo "$input" | jq -r '.title')
memory_type=$(echo "$input" | jq -r '.memory_type')
tags=$(echo "$input" | jq -r '.tags | join(" ")')
aliases=$(echo "$input" | jq -r '.aliases | map("\"" + . + "\"") | join(" ")')
content=$(echo "$input" | jq -r '.content')

ORG_ROAM_DIR="${ORG_ROAM_DIR:-$HOME/Library/Mobile Documents/com~apple~CloudDocs/notes/roam}"

# Generate UUID and timestamp
id=$(uuidgen)
timestamp=$(date "+%Y-%m-%d %H:%M")
slug=$(echo "$title" | tr '[:upper:]' '[:lower:]' | tr -s ' ' '-' | tr -cd '[:alnum:]-')
filename=$(date "+%Y-%m-%d-%H-%M-%S").$slug.org

# Build org file
cat > "$ORG_ROAM_DIR/$filename" <<EOF
:PROPERTIES:
:ID: $id
:ROAM_ALIASES: $aliases
:CREATED: <$timestamp>
:LAST_MODIFIED: <$timestamp>
:END:
#+TITLE: $title
#+FILETAGS: $memory_type $tags

$content
EOF

# Output JSON response
jq -n \
  --arg id "$id" \
  --arg file "$ORG_ROAM_DIR/$filename" \
  --arg title "$title" \
  '{id: $id, file: $file, title: $title}'
