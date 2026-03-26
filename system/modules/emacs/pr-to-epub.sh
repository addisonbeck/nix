#!/usr/bin/env bash
set -euo pipefail

PR_URL="${1:-}"
if [[ -z "$PR_URL" ]]; then
  echo "Usage: pr-to-epub <PR_URL|PR_NUMBER>" >&2
  exit 1
fi

OUTPUT_DIR="${HOME}/notes/epubs"
mkdir -p "$OUTPUT_DIR"

WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT

echo "Fetching PR metadata..."
PR_JSON=$(@gh@ pr view "$PR_URL" \
  --json title,body,author,state,number,headRefName,baseRefName,createdAt,headRepository,headRepositoryOwner,additions,deletions,changedFiles)

TITLE=$(@jq@ -r '.title' <<< "$PR_JSON")
BODY=$(@jq@ -r '.body // ""' <<< "$PR_JSON")
AUTHOR=$(@jq@ -r '.author.login' <<< "$PR_JSON")
STATE=$(@jq@ -r '.state' <<< "$PR_JSON")
NUMBER=$(@jq@ -r '.number' <<< "$PR_JSON")
REPO=$(@jq@ -r '.headRepositoryOwner.login + "/" + .headRepository.name' <<< "$PR_JSON")
HEAD=$(@jq@ -r '.headRefName' <<< "$PR_JSON")
BASE=$(@jq@ -r '.baseRefName' <<< "$PR_JSON")
ADDITIONS=$(@jq@ -r '.additions' <<< "$PR_JSON")
DELETIONS=$(@jq@ -r '.deletions' <<< "$PR_JSON")
CHANGED_FILES=$(@jq@ -r '.changedFiles' <<< "$PR_JSON")
CREATED=$(@jq@ -r '.createdAt' <<< "$PR_JSON")
DATE="${CREATED%%T*}"

echo "Fetching commits..."
COMMITS_MD=$(@gh@ pr view "$PR_URL" \
  --json commits \
  --jq '.commits[] | "- **" + .messageHeadline + "**" + (if .messageBody != "" then "\n\n  " + (.messageBody | gsub("\n"; "\n  ")) else "" end)' \
  2>/dev/null) || COMMITS_MD="*(commits unavailable)*"

echo "Fetching changed files..."
FILES_MD=$(@gh@ pr view "$PR_URL" \
  --json files \
  --jq '.files[] | "- `" + .path + "` (" + .status + ", +" + (.additions|tostring) + " / \u2212" + (.deletions|tostring) + ")"' \
  2>/dev/null) || FILES_MD="*(file list unavailable)*"

echo "Fetching diff..."
DIFF=$(@gh@ pr diff "$PR_URL" 2>/dev/null) || DIFF="*(diff unavailable)*"

# Build filename slug (max 60 chars)
slugify() {
  printf '%s' "$1" | tr '[:upper:]' '[:lower:]' | tr -cs 'a-z0-9' '-' | sed 's/^-*//;s/-*$//' | cut -c1-60
}
SLUG="pr-$(slugify "${NUMBER}-${TITLE}")"
EPUB="${OUTPUT_DIR}/${SLUG}.epub"

# Escape double-quotes for YAML
TITLE_SAFE="${TITLE//\"/\\\"}"

MD_FILE="${WORKDIR}/pr.md"

{
  printf -- '---\ntitle: "PR #%s: %s"\nauthor: "%s"\ndate: "%s"\n---\n\n' \
    "$NUMBER" "$TITLE_SAFE" "$AUTHOR" "$DATE"

  printf '## Overview\n\n'
  printf '|  |  |\n|:--|:--|\n'
  printf '| **Repository** | `%s` |\n' "$REPO"
  printf '| **State** | `%s` |\n' "$STATE"
  printf '| **Branch** | `%s` \u2192 `%s` |\n' "$HEAD" "$BASE"
  printf '| **Author** | @%s |\n' "$AUTHOR"
  printf '| **Created** | %s |\n' "$DATE"
  printf '| **Changes** | +%s / \u2212%s across %s files |\n\n' "$ADDITIONS" "$DELETIONS" "$CHANGED_FILES"

  printf '## Description\n\n'
  printf '%s\n\n' "$BODY"

  printf '## Commits\n\n'
  printf '%s\n\n' "$COMMITS_MD"

  printf '## Changed Files\n\n'
  printf '%s\n\n' "$FILES_MD"

  printf '## Diff\n\n'
  printf '~~~~{.diff}\n'
  printf '%s\n' "$DIFF"
  printf '~~~~\n'
} > "$MD_FILE"

echo "Generating EPUB..."
@pandoc@ \
  --from=markdown+smart \
  --to=epub3 \
  --output="$EPUB" \
  --css="@pr-review-css@" \
  --highlight-style=pygments \
  --epub-embed-font="@iosevka-regular@" \
  --epub-embed-font="@iosevka-bold@" \
  --epub-embed-font="@iosevka-italic@" \
  --toc \
  --toc-depth=2 \
  --metadata lang=en \
  "$MD_FILE"

echo "Saved: $EPUB"
