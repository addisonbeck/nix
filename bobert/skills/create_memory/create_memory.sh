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
subfolder=$(echo "$input" | jq -r '.subfolder // ""')

ORG_ROAM_DIR="${ORG_ROAM_DIR:-$HOME/Library/Mobile Documents/com~apple~CloudDocs/notes/roam}"

# Validate memory_type against allowed values
valid_types=("episodic" "semantic" "procedural" "associative" "working" "reflective" "reference")
  # shellcheck disable=SC2076
  if [[ ! " ${valid_types[*]} " =~ " ${memory_type} " ]]; then
  jq -n \
    --arg error "Invalid memory_type: '$memory_type'. Must be one of: episodic, semantic, procedural, associative, working, reflective, reference" \
    '{error: $error}' >&2
  exit 1
fi

# Generate UUID and timestamp
id=$(uuidgen)
timestamp=$(date "+%Y-%m-%d %H:%M")
slug=$(echo "$title" | tr '[:upper:]' '[:lower:]' | tr -s ' ' '-' | tr -cd '[:alnum:]-')
filename=$(date "+%Y-%m-%d-%H-%M-%S").$slug.org

# Build file path (with optional subfolder)
if [ -n "$subfolder" ]; then
  filepath="$ORG_ROAM_DIR/$subfolder/$filename"
else
  filepath="$ORG_ROAM_DIR/$filename"
fi

# Build org file
cat > "$filepath" <<EOF
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

# Log to Bobert's inbox
INBOX_FILE="$HOME/Library/Mobile Documents/com~apple~CloudDocs/notes/roam/boberts-inbox.org"
echo "- [[id:$id][$title]]" >> "$INBOX_FILE"

# Output JSON response
jq -n \
  --arg id "$id" \
  --arg file "$filepath" \
  --arg title "$title" \
  '{id: $id, file: $file, title: $title}'
