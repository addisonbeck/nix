#!/usr/bin/env bash
# PreToolUse hook: blocks Bash write operations targeting org-roam directory
#
# Catches cp/mv/rsync/tee/sed-i/perl-i and redirect operators targeting
# /roam/ paths, forcing agents to use create_memory skill instead.
#
# Input:  JSON on stdin, format: {"tool_name": "Bash", "tool_input": {"command": "..."}}
# Output: JSON permissionDecision to stdout

set -euo pipefail

hook_input=$(cat)
command=$(echo "$hook_input" | jq -r '.tool_input.command // empty' 2>/dev/null)

# Fast path: empty command, allow
if [ -z "$command" ]; then
  echo '{"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "allow"}}'
  exit 0
fi

# Check if command references org-roam directory (by path or variable name)
targets_roam=false
if echo "$command" | grep -q '/roam/' || echo "$command" | grep -q 'ORG_ROAM_DIR'; then
  targets_roam=true
fi

if [ "$targets_roam" = false ]; then
  echo '{"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "allow"}}'
  exit 0
fi

# Check if command contains write-capable operations
# shellcheck disable=SC2016
write_pattern='(^|[[:space:]])(cp|mv|rsync|install|tee|python)[[:space:]]|sed[[:space:]]+-i|perl[[:space:]]+-i|>[[:space:]]*(\$ORG_ROAM_DIR|[^[:space:]]*\/roam\/)'

if echo "$command" | grep -qE "$write_pattern"; then
  jq -n \
    --arg reason "Direct Bash write operations on org-roam nodes are blocked. Use the create_memory skill instead: invoke it via the Skill tool. Direct writes bypass UUID generation and backlink management required for org-roam integrity." \
    '{
      "hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": "deny",
        "permissionDecisionReason": $reason
      }
    }'
  exit 0
fi

# No write operation detected, allow
echo '{"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "allow"}}'
exit 0
