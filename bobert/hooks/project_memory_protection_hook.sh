#!/usr/bin/env bash
# PreToolUse hook for project memory protection
# Denies Read operations on paths containing /.claude/projects/ AND /memory/
# Guides users to read_memory skill as the correct alternative

set -euo pipefail

ORG_ROAM_DIR="${ORG_ROAM_DIR:-$HOME/Library/Mobile Documents/com~apple~CloudDocs/notes/roam}"

# Parse hook input JSON from stdin
# Expected format: {"tool_name": "Read", "tool_input": {"file_path": "...", ...}}
hook_input=$(cat)

# Extract file_path from tool_input using jq
# Returns empty string if file_path doesn't exist (safe passthrough)
file_path=$(echo "$hook_input" | jq -r '.tool_input.file_path // empty')

# If no file_path present, allow operation (not a file operation)
if [ -z "$file_path" ]; then
  echo '{"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "allow"}}'
  exit 0
fi

# Check if file_path contains BOTH /.claude/projects/ AND /memory/ substrings
if [[ "$file_path" == *"/.claude/projects/"* ]] && [[ "$file_path" == *"/memory/"* ]]; then
  # Deny operation with structured guidance message
  jq -n \
    --arg reason "⚠️ Direct file access to project memory is not allowed.

The Read tool cannot access ~/.claude/projects/.../memory/ paths.
Use the read_memory skill instead:

  ~/.claude/skills/read_memory/read_memory.sh <UUID>

Memory is stored in org-roam directory: $ORG_ROAM_DIR" \
    '{
      "hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": "deny",
        "permissionDecisionReason": $reason
      }
    }'
  exit 0
fi

# Path does not match protection pattern, allow operation
echo '{"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "allow"}}'
exit 0
