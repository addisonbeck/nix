#!/usr/bin/env bash
# execute-commit.sh — final execution step for the write-git-commit skill
#
# Receives a fully-formed commit message on stdin, creates the commit, and
# verifies it with git show HEAD.
#
# IMPORTANT: Staging is NOT done by this script. The caller must explicitly
# stage files (e.g., git add path/to/file) before invoking this script.
#
# Usage:
#   cat <<'EOF' | ~/.claude/skills/write-git-commit/execute-commit.sh
#   feat(scope): subject line
#
#   Body explaining why.
#   EOF
#
# Running git commit as a subprocess here keeps it below the PreToolUse
# hook that blocks direct Bash(git commit) tool calls from Claude.

set -euo pipefail

commit_message=$(cat)

if [ -z "$commit_message" ]; then
  echo "Error: no commit message on stdin" >&2
  exit 1
fi

git commit --no-gpg-sign -m "$commit_message"

git show HEAD
