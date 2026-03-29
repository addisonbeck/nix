#!/usr/bin/env bash
# ~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh
#
# Orchestrates git interactive rebases programmatically through GIT_SEQUENCE_EDITOR
# and edit+amend patterns. Supports squash, reword, drop, fixup, and reorder operations.

set -euo pipefail

# Parse JSON input from stdin
input=$(cat)

# Extract input fields
base_commit=$(echo "$input" | jq -r '.base_commit')
operations_json=$(echo "$input" | jq -c '.operations')

# Validate required fields
if [ "$base_commit" = "null" ] || [ -z "$base_commit" ]; then
  jq -n --arg error "base_commit is required" '{error: $error}' >&2
  exit 1
fi

if [ "$operations_json" = "null" ] || [ "$operations_json" = "[]" ]; then
  jq -n --arg error "operations array is required and must not be empty" '{error: $error}' >&2
  exit 1
fi

# Verify we're in a git repository
if ! git rev-parse --git-dir >/dev/null 2>&1; then
  jq -n --arg error "Not in a git repository" '{error: $error}' >&2
  exit 1
fi

# Verify working tree is clean
if ! git diff-index --quiet HEAD -- 2>/dev/null; then
  jq -n --arg error "Working tree has uncommitted changes. Commit or stash changes before rebasing." '{error: $error}' >&2
  exit 1
fi

# Get current branch name
current_branch=$(git branch --show-current)
if [ -z "$current_branch" ]; then
  jq -n --arg error "Not on a branch (detached HEAD). Checkout a branch before rebasing." '{error: $error}' >&2
  exit 1
fi

# Verify base commit exists
if ! git rev-parse --verify "$base_commit" >/dev/null 2>&1; then
  jq -n --arg error "Base commit '$base_commit' not found" '{error: $error}' >&2
  exit 1
fi

# Parse and validate operations
operations_count=$(echo "$operations_json" | jq 'length')
reword_commits=()
reword_messages=()
edit_commits=()

for (( i=0; i<operations_count; i++ )); do
  operation=$(echo "$operations_json" | jq -c ".[$i]")
  commit=$(echo "$operation" | jq -r '.commit')
  action=$(echo "$operation" | jq -r '.action')
  message=$(echo "$operation" | jq -r '.message // ""')

  # Validate commit exists
  if ! git rev-parse --verify "$commit" >/dev/null 2>&1; then
    available=$(git log --oneline "$base_commit..HEAD" | head -5)
    jq -n \
      --arg error "Commit '$commit' not found in current branch history" \
      --arg details "Available commits:\n$available" \
      '{error: $error, details: $details}' >&2
    exit 1
  fi

  # Validate action
  case "$action" in
    pick|drop|reword|squash|fixup|edit)
      ;;
    *)
      jq -n \
        --arg error "Invalid action '$action' for commit $commit" \
        --arg details "Valid actions: pick, drop, reword, squash, fixup, edit" \
        '{error: $error, details: $details}' >&2
      exit 1
      ;;
  esac

  # Validate reword has message
  if [ "$action" = "reword" ] && [ -z "$message" ]; then
    jq -n \
      --arg error "Reword action for commit $commit requires a message field" \
      '{error: $error}' >&2
    exit 1
  fi

  # Store reword commits and messages for edit+amend workflow
  if [ "$action" = "reword" ]; then
    reword_commits+=("$commit")
    reword_messages+=("$message")
  fi

  # Store edit commits for user-facing pause detection
  if [ "$action" = "edit" ]; then
    edit_commits+=("$commit")
  fi
done

# Create temporary directory for scripts with trap cleanup
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

SEQ_EDITOR_SCRIPT="$TEMP_DIR/sequence-editor.sh"
REWORD_DATA="$TEMP_DIR/reword-data.json"
EDIT_DATA="$TEMP_DIR/edit-data.json"

# Build reword data file for later use
echo "$operations_json" | jq '[.[] | select(.action == "reword") | {commit: .commit, message: .message}]' > "$REWORD_DATA"

# Build edit commits list for pause detection
echo "$operations_json" | jq '[.[] | select(.action == "edit") | .commit]' > "$EDIT_DATA"

# Create sequence editor script
cat > "$SEQ_EDITOR_SCRIPT" <<'EDITOR_SCRIPT_EOF'
#!/usr/bin/env bash
set -euo pipefail

TODO_FILE="$1"

# Read the operations from the calling script's environment
# The operations are passed via a JSON file path in REBASE_OPERATIONS_JSON env var
OPERATIONS_JSON="$REBASE_OPERATIONS_JSON"

# Parse operations and build new todo list
NEW_TODO=""
operations_count=$(jq 'length' < "$OPERATIONS_JSON")

for (( i=0; i<operations_count; i++ )); do
  operation=$(jq -c ".[$i]" < "$OPERATIONS_JSON")
  commit=$(echo "$operation" | jq -r '.commit')
  action=$(echo "$operation" | jq -r '.action')

  # For reword, use edit action (we'll amend the message later)
  if [ "$action" = "reword" ]; then
    action="edit"
  fi

  # Get the full commit hash
  full_hash=$(git rev-parse "$commit")
  short_hash="${full_hash:0:7}"

  # Get commit subject
  subject=$(git log -1 --format=%s "$full_hash")

  # Append to new todo list
  NEW_TODO="${NEW_TODO}${action} ${short_hash} ${subject}\n"
done

# Write new todo list to the todo file
echo -e "$NEW_TODO" > "$TODO_FILE"
EDITOR_SCRIPT_EOF

chmod +x "$SEQ_EDITOR_SCRIPT"

# Export the operations JSON path for the sequence editor
export REBASE_OPERATIONS_JSON="$TEMP_DIR/operations.json"
echo "$operations_json" > "$REBASE_OPERATIONS_JSON"

# Set GIT_SEQUENCE_EDITOR to our script
export GIT_SEQUENCE_EDITOR="$SEQ_EDITOR_SCRIPT"

# Execute the rebase
# Using GIT_EDITOR=true suppresses any unexpected editor invocations
if ! GIT_EDITOR=true git rebase -i "$base_commit" 2>"$TEMP_DIR/rebase-stderr.log"; then
  # Check if we're in a conflict state
  if git rev-parse --verify REBASE_HEAD >/dev/null 2>&1; then
    # We're in a conflict state
    conflict_commit=$(git rev-parse REBASE_HEAD)
    conflicting_files=$(git diff --name-only --diff-filter=U)

    # Find which action caused the conflict
    conflict_action="unknown"
    for (( i=0; i<operations_count; i++ )); do
      operation=$(echo "$operations_json" | jq -c ".[$i]")
      commit=$(echo "$operation" | jq -r '.commit')
      if git rev-parse "$commit" | grep -q "$(git rev-parse REBASE_HEAD 2>/dev/null || echo '')"; then
        conflict_action=$(echo "$operation" | jq -r '.action')
        break
      fi
    done

    jq -n \
      --arg status "paused" \
      --arg reason "conflict" \
      --arg commit "$conflict_commit" \
      --arg action "$conflict_action" \
      --argjson files "$(echo "$conflicting_files" | jq -R . | jq -s .)" \
      --arg instructions "Resolve conflicts in the listed files, then stage them with 'git add' and run the continue command" \
      --arg continue_cmd "git add <resolved-files> && git rebase --continue" \
      --arg abort_cmd "git rebase --abort" \
      '{
        status: $status,
        reason: $reason,
        commit: $commit,
        action: $action,
        conflicting_files: $files,
        instructions: $instructions,
        continue_command: $continue_cmd,
        abort_command: $abort_cmd
      }'
    exit 0
  else
    # Some other error occurred
    stderr_content=$(cat "$TEMP_DIR/rebase-stderr.log" 2>/dev/null || echo "")
    jq -n \
      --arg error "Rebase failed" \
      --arg details "$stderr_content" \
      '{error: $error, details: $details}' >&2
    exit 1
  fi
fi

# Handle edit actions (for reword operations and user edit pauses)
# Check if we're paused at an edit
while git rev-parse --verify REBASE_HEAD >/dev/null 2>&1; do
  current_commit=$(git rev-parse REBASE_HEAD)
  current_commit_short="${current_commit:0:7}"

  # Check if this is a user-facing edit action (not a reword)
  is_user_edit=$(jq -r --arg commit "$current_commit" --arg short "$current_commit_short" \
    'any(.[] == $commit or .[] == $short)' \
    < "$EDIT_DATA")

  if [ "$is_user_edit" = "true" ]; then
    # This is a user edit action - return control to user
    jq -n \
      --arg status "paused" \
      --arg reason "edit" \
      --arg commit "$current_commit" \
      --arg action "edit" \
      --arg instructions "Rebase paused at commit $current_commit_short for editing. Make your changes (modify files, split commits, add commits, etc.), then run the continue command when ready." \
      --arg continue_cmd "git rebase --continue" \
      --arg abort_cmd "git rebase --abort" \
      '{
        status: $status,
        reason: $reason,
        commit: $commit,
        action: $action,
        instructions: $instructions,
        continue_command: $continue_cmd,
        abort_command: $abort_cmd
      }'
    exit 0
  fi

  # Check if this commit has a reword operation
  reword_entry=$(jq -r --arg commit "$current_commit" --arg short "$current_commit_short" \
    '.[] | select(.commit == $commit or .commit == $short or ($commit | startswith(.commit))) | .message' \
    < "$REWORD_DATA")

  if [ -n "$reword_entry" ] && [ "$reword_entry" != "null" ]; then
    # This is a reword operation - amend the message automatically
    if ! git commit --amend -m "$reword_entry" 2>"$TEMP_DIR/amend-stderr.log"; then
      stderr_content=$(cat "$TEMP_DIR/amend-stderr.log" 2>/dev/null || echo "")
      jq -n \
        --arg error "Failed to amend commit message" \
        --arg details "$stderr_content" \
        '{error: $error, details: $details}' >&2
      exit 1
    fi
  fi

  # Continue the rebase
  if ! git rebase --continue 2>"$TEMP_DIR/continue-stderr.log"; then
    # Check if we hit a conflict
    if git rev-parse --verify REBASE_HEAD >/dev/null 2>&1; then
      conflict_commit=$(git rev-parse REBASE_HEAD)
      conflicting_files=$(git diff --name-only --diff-filter=U)

      # Find which action caused the conflict
      conflict_action="unknown"
      for (( i=0; i<operations_count; i++ )); do
        operation=$(echo "$operations_json" | jq -c ".[$i]")
        commit=$(echo "$operation" | jq -r '.commit')
        if git rev-parse "$commit" 2>/dev/null | grep -q "$(echo "$conflict_commit" | head -c 7)"; then
          conflict_action=$(echo "$operation" | jq -r '.action')
          break
        fi
      done

      jq -n \
        --arg status "paused" \
        --arg reason "conflict" \
        --arg commit "$conflict_commit" \
        --arg action "$conflict_action" \
        --argjson files "$(echo "$conflicting_files" | jq -R . | jq -s .)" \
        --arg instructions "Resolve conflicts in the listed files, then stage them with 'git add' and run the continue command" \
        --arg continue_cmd "git add <resolved-files> && git rebase --continue" \
        --arg abort_cmd "git rebase --abort" \
        '{
          status: $status,
          reason: $reason,
          commit: $commit,
          action: $action,
          conflicting_files: $files,
          instructions: $instructions,
          continue_command: $continue_cmd,
          abort_command: $abort_cmd
        }'
      exit 0
    else
      # Rebase completed
      break
    fi
  fi
done

# Rebase completed successfully
final_head=$(git rev-parse HEAD)
commits_processed="$operations_count"

jq -n \
  --arg status "success" \
  --arg base "$base_commit" \
  --arg branch "$current_branch" \
  --argjson processed "$commits_processed" \
  --arg head "$final_head" \
  '{
    status: $status,
    base_commit: $base,
    branch: $branch,
    commits_processed: $processed,
    final_head: $head
  }'
