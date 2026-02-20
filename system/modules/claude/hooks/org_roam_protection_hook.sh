#!/usr/bin/env bash
# PreToolUse hook for org-roam knowledge graph protection
# Denies Edit/Write operations on paths containing /roam/ substring
# Guides users to create_memory skill as the correct alternative

set -euo pipefail

ORG_ROAM_DIR="${ORG_ROAM_DIR:-$HOME/Library/Mobile Documents/com~apple~CloudDocs/notes/roam}"

# Parse hook input JSON from stdin
# Expected format: {"tool_name": "Edit", "tool_input": {"file_path": "...", ...}}
hook_input=$(cat)

# Extract file_path from tool_input using jq
# Returns empty string if file_path doesn't exist (safe passthrough)
file_path=$(echo "$hook_input" | jq -r '.tool_input.file_path // empty')

# If no file_path present, allow operation (not a file operation)
if [ -z "$file_path" ]; then
  echo '{"permissionDecision": "allow"}'
  exit 0
fi

# Check if file_path contains /roam/ substring
if [[ "$file_path" == *"/roam/"* ]]; then
  # Deny operation with structured guidance message
  jq -n \
    --arg reason "Direct file operations on org-roam nodes bypass UUID and backlink management. Use the create_memory skill instead: Skill 'create_memory'" \
    '{
      "permissionDecision": "deny",
      "permissionDecisionReason": $reason
    }'
  exit 0
fi

# Path does not contain /roam/, allow operation
echo '{"permissionDecision": "allow"}'
exit 0
