#!/usr/bin/env bash
# PreToolUse hook: blocks direct git commit tool calls
#
# Forces all git commits through the write-git-commit skill.
# The skill's execute-commit.sh script runs git commit as a subprocess,
# which is invisible to PreToolUse hooks — so the skill path always works.
#
# Input:  JSON on stdin, format: {"tool_name": "Bash", "tool_input": {"command": "..."}}
# Output: JSON permissionDecision to stdout

set -euo pipefail

hook_input=$(cat)
command=$(echo "$hook_input" | jq -r '.tool_input.command // empty' 2>/dev/null)

# Fast path: not a git commit, allow
if ! echo "$command" | grep -qE 'git[[:space:]]+commit'; then
  echo '{"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "allow"}}'
  exit 0
fi

# Block: direct git commit detected
jq -n \
  --arg reason "Direct git commit is blocked. Use the write-git-commit skill instead: invoke it via the Skill tool, which will call execute-commit.sh to run the actual commit. This ensures the full commit workflow (diff analysis, message generation, pre-commit checks) runs before any commit executes." \
  '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": $reason
    }
  }'
exit 0
