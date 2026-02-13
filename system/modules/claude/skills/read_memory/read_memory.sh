#!/usr/bin/env bash
# ~/.claude/skills/read_memory/read_memory.sh

set -euo pipefail

ID="$1"
ORG_ROAM_DIR="${ORG_ROAM_DIR:-$HOME/Library/Mobile Documents/com~apple~CloudDocs/notes/roam}"

# Find file containing ID in properties drawer (within first 20 lines)
file=$(grep -r "^:ID: $ID$" "$ORG_ROAM_DIR" -l \
  --include="*.org" | head -1 || true)

if [ -z "$file" ]; then
  jq -n --arg error "Node with ID $ID not found" '{error: $error}' >&2
  exit 1
fi

# Extract title from #+TITLE directive
title=$(grep "^#+TITLE:" "$file" | cut -d: -f2- | xargs || echo "Untitled")

# Read full file content
content=$(cat "$file")

# Output JSON with proper escaping
jq -n \
  --arg id "$ID" \
  --arg title "$title" \
  --arg content "$content" \
  --arg file "$file" \
  '{id: $id, title: $title, content: $content, file: $file}'
