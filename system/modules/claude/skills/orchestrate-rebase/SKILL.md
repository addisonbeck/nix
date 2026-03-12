---
name: orchestrate-rebase
description: |
  Orchestrates git interactive rebases programmatically through GIT_SEQUENCE_EDITOR
  and edit+amend patterns. Supports squash, reword, drop, fixup, edit, and reorder operations
  without requiring stdin interaction. Edit action pauses for manual changes (file modifications,
  commit splitting, etc.). Handles conflicts gracefully with clear resolution paths.
allowed-tools: Bash(~/.claude/skills/rebase-orchestrate/*)
---

# orchestrate-rebase Skill

Orchestrates git interactive rebases programmatically without stdin interaction. Uses GIT_SEQUENCE_EDITOR to control the rebase todo list and the edit+amend pattern for commit message rewording.

When invoked, pipe JSON input to the bundled rebase-orchestrate.sh script:

```bash
echo '$ARGUMENTS' | ~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh
```

This skill enables LLM agents to programmatically control git interactive rebases (squash, reword, drop, fixup, edit, reorder commits) through environment variable overrides rather than stdin interaction. The edit action enables pausing at specific commits for arbitrary file modifications, commit splitting, or adding additional commits within the rebase workflow.

## Input Contract

The JSON input must contain:

- **base_commit** (string, required): Base commit to rebase onto. Examples: "main", "HEAD~5", "abc1234"
- **operations** (array, required): List of rebase operations, each containing:
  - **commit** (string, required): Commit hash (short or full SHA)
  - **action** (string, required): One of: "pick", "drop", "reword", "squash", "fixup", "edit"
  - **message** (string, optional): New commit message (required for "reword" action, not used for "edit")

### Validation Rules

- At least one operation must be specified
- All commits must exist in the current branch history
- Actions must be valid git rebase commands
- Reword actions must include a message field
- Edit actions do not require a message field (user will make changes at pause)
- Operations are applied in the order specified in the array

### Example Input

```json
{
  "base_commit": "HEAD~5",
  "operations": [
    {"commit": "abc1234", "action": "pick"},
    {"commit": "def5678", "action": "reword", "message": "feat: improved error handling"},
    {"commit": "ghi9012", "action": "edit"},
    {"commit": "jkl3456", "action": "drop"}
  ]
}
```

## Output Contract

### Success Response

- **status** (string): "success"
- **base_commit** (string): The base commit used for the rebase
- **branch** (string): Current branch name
- **commits_processed** (number): Number of commits successfully processed
- **final_head** (string): Final HEAD commit SHA after rebase

### Paused Response (Edit or Conflict)

- **status** (string): "paused"
- **reason** (string): Why rebase is paused ("edit" or "conflict")
- **commit** (string): Commit SHA where rebase paused
- **action** (string): The rebase action at this pause point
- **conflicting_files** (array, optional): List of files with conflicts (only for conflict pauses)
- **instructions** (string): Human-readable instructions for what to do next
- **continue_command** (string): Command to continue after making changes
- **abort_command** (string): Command to abort the rebase

### Conflict Response (Legacy - now uses Paused Response)

- **status** (string): "conflict"
- **commit** (string): Commit SHA where conflict occurred
- **action** (string): The rebase action that caused the conflict
- **conflicting_files** (array): List of files with conflicts
- **instructions** (string): Human-readable instructions for resolution
- **continue_command** (string): Command to continue after resolving
- **abort_command** (string): Command to abort the rebase

### Error Response

- **error** (string): Error message describing what went wrong
- **details** (string, optional): Additional context about the failure

### Examples

Success:
```json
{
  "status": "success",
  "base_commit": "main",
  "branch": "feature/auth",
  "commits_processed": 4,
  "final_head": "abc123def456"
}
```

Paused for Edit:
```json
{
  "status": "paused",
  "reason": "edit",
  "commit": "ghi9012",
  "action": "edit",
  "instructions": "Rebase paused at commit ghi9012 for editing. Make your changes (modify files, split commits, etc.), then run the continue command when ready.",
  "continue_command": "git rebase --continue",
  "abort_command": "git rebase --abort"
}
```

Paused for Conflict:
```json
{
  "status": "paused",
  "reason": "conflict",
  "commit": "def5678",
  "action": "squash",
  "conflicting_files": ["src/auth.ts", "src/types.ts"],
  "instructions": "Resolve conflicts in the listed files, then stage them with 'git add' and run the continue command",
  "continue_command": "git add <resolved-files> && git rebase --continue",
  "abort_command": "git rebase --abort"
}
```

Error:
```json
{
  "error": "Commit abc1234 not found in current branch history",
  "details": "Available commits: HEAD~1 (def5678), HEAD~2 (ghi9012), HEAD~3 (jkl3456)"
}
```

## Implementation Architecture

### Implementation Type
Bash script using GIT_SEQUENCE_EDITOR environment variable override and edit+amend pattern for rewording.

### Script Structure
1. **Input parsing**: Parse JSON from stdin using jq
2. **Validation**: Verify all commits exist, actions are valid, working tree is clean
3. **Todo list generation**: Build rebase todo list based on operations array
4. **Sequence editor script**: Create temporary script for GIT_SEQUENCE_EDITOR
5. **Rebase execution**: Run git rebase -i with environment overrides
6. **Edit handling**: For reword actions, pause at edit and amend commit message automatically; for user edit actions, return control to user
7. **Conflict detection**: Check for REBASE_HEAD to detect conflicts
8. **Output generation**: Return JSON status with appropriate response schema

### Key Patterns

- **Error handling**: `set -euo pipefail` at script start
- **JSON processing**: jq for both input parsing and output generation
- **Temporary files**: mktemp with trap-based cleanup for sequence editor scripts
- **Conflict detection**: `git rev-parse --verify REBASE_HEAD` exit code check
- **State inspection**: Parse `.git/rebase-merge/` directory for rebase state
- **Edit+amend workflow**: Use `edit` action instead of `reword`, then `git commit --amend -m` at pause
- **GIT_SEQUENCE_EDITOR override**: Export custom script path to control todo list

### Workflow Strategy

The script implements the primary workflow recommended by the Learning Packet (EA01604A-CFA5-48CB-B61F-6DB9FDB89AFC):

1. Generate bash script with desired todo list transformations
2. Set `GIT_SEQUENCE_EDITOR` to script path
3. Run `git rebase -i <base>`
4. For reword operations, use `edit` action internally and run `git commit --amend -m "new message"` automatically
5. For user edit operations, pause and return control to user with instructions
6. Continue with `git rebase --continue` (automatic for reword, manual for edit)
7. Detect conflicts via `git rev-parse --verify REBASE_HEAD`

This approach avoids stdin interaction entirely while providing full control over the rebase sequence. User edit actions enable arbitrary file modifications, commit splitting, or additional commits within the rebase workflow.

## Environment Dependencies

- **Git**: Git 2.26+ required (for modern rebase features)
- **jq**: JSON processing for input/output
- **bash**: Version 4+ for array support and modern features
- **mktemp**: Secure temporary file creation
- **Working directory**: Must be inside a git repository with clean working tree

### Platform Requirements
- macOS and Linux supported
- Requires write access to `.git/` directory
- Requires ability to set environment variables

## Usage & Testing Guidance

### Direct Testing

Test successful rebase (squash last two commits):
```bash
# Create test repository with commits
git init /tmp/rebase-test && cd /tmp/rebase-test
git config user.email "test@example.com" && git config user.name "Test"
echo "1" > file.txt && git add . && git commit -m "commit 1"
echo "2" >> file.txt && git add . && git commit -m "commit 2"
echo "3" >> file.txt && git add . && git commit -m "commit 3"

# Get commit hashes
COMMIT1=$(git log --oneline | sed -n '3p' | awk '{print $1}')
COMMIT2=$(git log --oneline | sed -n '2p' | awk '{print $1}')
COMMIT3=$(git log --oneline | sed -n '1p' | awk '{print $1}')

# Test the skill - squash commit 2 into commit 1, keep commit 3
echo "{
  \"base_commit\": \"HEAD~3\",
  \"operations\": [
    {\"commit\": \"$COMMIT1\", \"action\": \"pick\"},
    {\"commit\": \"$COMMIT2\", \"action\": \"squash\"},
    {\"commit\": \"$COMMIT3\", \"action\": \"pick\"}
  ]
}" | ~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh
```

Test reword operation:
```bash
# Using the same test repository
echo "{
  \"base_commit\": \"HEAD~3\",
  \"operations\": [
    {\"commit\": \"$COMMIT1\", \"action\": \"pick\"},
    {\"commit\": \"$COMMIT2\", \"action\": \"reword\", \"message\": \"feat: improved implementation\"},
    {\"commit\": \"$COMMIT3\", \"action\": \"pick\"}
  ]
}" | ~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh
```

Test error handling (invalid commit):
```bash
echo "{
  \"base_commit\": \"HEAD~2\",
  \"operations\": [
    {\"commit\": \"invalid123\", \"action\": \"pick\"}
  ]
}" | ~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh
```

Test validation (missing message for reword):
```bash
echo "{
  \"base_commit\": \"HEAD~2\",
  \"operations\": [
    {\"commit\": \"$COMMIT1\", \"action\": \"reword\"}
  ]
}" | ~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh
```

Test edit action (pause for manual changes):
```bash
# Using the same test repository
echo "{
  \"base_commit\": \"HEAD~3\",
  \"operations\": [
    {\"commit\": \"$COMMIT1\", \"action\": \"pick\"},
    {\"commit\": \"$COMMIT2\", \"action\": \"edit\"},
    {\"commit\": \"$COMMIT3\", \"action\": \"pick\"}
  ]
}" | ~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh

# Expected output: status "paused", reason "edit", with instructions to continue
# At this point, you can:
# - Modify files: vim file.txt && git add file.txt && git commit --amend
# - Split commit: git reset HEAD^ && git add -p && git commit -m "part 1" && git commit -am "part 2"
# - Add commits: echo "4" >> file.txt && git add . && git commit -m "additional change"
# Then continue: git rebase --continue
```

### Claude Code Invocation

Basic usage (squash last 3 commits):
```
/rebase-orchestrate {
  "base_commit": "HEAD~3",
  "operations": [
    {"commit": "abc1234", "action": "pick"},
    {"commit": "def5678", "action": "squash"},
    {"commit": "ghi9012", "action": "squash"}
  ]
}
```

Reword commit message:
```
/rebase-orchestrate {
  "base_commit": "main",
  "operations": [
    {"commit": "abc1234", "action": "reword", "message": "feat: add user authentication"}
  ]
}
```

Drop unwanted commits:
```
/rebase-orchestrate {
  "base_commit": "HEAD~5",
  "operations": [
    {"commit": "abc1234", "action": "pick"},
    {"commit": "def5678", "action": "drop"},
    {"commit": "ghi9012", "action": "pick"},
    {"commit": "jkl3456", "action": "drop"},
    {"commit": "mno7890", "action": "pick"}
  ]
}
```

Pause for manual editing:
```
/rebase-orchestrate {
  "base_commit": "HEAD~3",
  "operations": [
    {"commit": "abc1234", "action": "pick"},
    {"commit": "def5678", "action": "edit"},
    {"commit": "ghi9012", "action": "pick"}
  ]
}
```

This will pause at commit def5678, allowing you to modify files, split the commit, or add additional commits before continuing.

### Installation

```bash
# Rebuild system to install skill
nix develop .#building --command rebuild <hostname>

# Verify installation
ls -la ~/.claude/skills/rebase-orchestrate/
~/.claude/skills/rebase-orchestrate/rebase-orchestrate.sh --version 2>/dev/null || echo "Script installed"
```

### Edit Workflow

When the skill returns a paused response with reason "edit":

1. The rebase has stopped at the specified commit
2. Make your changes:
   - Modify files: `vim file.txt`, `git add file.txt`, `git commit --amend`
   - Split commit: `git reset HEAD^`, stage and commit changes separately
   - Add more commits: make changes, `git add`, `git commit`
3. Continue the rebase: `git rebase --continue`
4. Alternatively, abort the rebase: `git rebase --abort`

The edit action enables arbitrary modifications within a commit, making it ideal for:
- Fixing issues in a specific commit without changing its position
- Splitting one large commit into multiple smaller commits
- Adding forgotten changes to an existing commit

### Conflict Resolution Workflow

When the skill returns a paused response with reason "conflict":

1. Examine the conflicting files listed in the response
2. Resolve conflicts manually using your editor or git mergetool
3. Stage resolved files: `git add <resolved-files>`
4. Continue the rebase: `git rebase --continue`
5. The rebase will proceed with remaining operations
6. Alternatively, abort the rebase: `git rebase --abort`

### Common Patterns

**Squash last N commits into one**:
```json
{
  "base_commit": "HEAD~5",
  "operations": [
    {"commit": "oldest", "action": "pick"},
    {"commit": "commit2", "action": "squash"},
    {"commit": "commit3", "action": "squash"},
    {"commit": "commit4", "action": "squash"},
    {"commit": "newest", "action": "squash"}
  ]
}
```

**Reorder commits** (change operation sequence):
```json
{
  "base_commit": "HEAD~3",
  "operations": [
    {"commit": "commit3", "action": "pick"},
    {"commit": "commit1", "action": "pick"},
    {"commit": "commit2", "action": "pick"}
  ]
}
```

**Clean up feature branch** (drop fixup commits, reword descriptions):
```json
{
  "base_commit": "main",
  "operations": [
    {"commit": "abc1234", "action": "reword", "message": "feat: implement user auth"},
    {"commit": "def5678", "action": "drop"},
    {"commit": "ghi9012", "action": "pick"},
    {"commit": "jkl3456", "action": "drop"}
  ]
}
```

**Pause for file modifications** (edit specific commits):
```json
{
  "base_commit": "HEAD~4",
  "operations": [
    {"commit": "abc1234", "action": "pick"},
    {"commit": "def5678", "action": "edit"},
    {"commit": "ghi9012", "action": "edit"},
    {"commit": "jkl3456", "action": "pick"}
  ]
}
```

Use this to pause at specific commits for manual modifications, file changes, or commit splitting.

### Expected Outcomes

- **Successful rebase**: Working tree is clean, branch history is rewritten, output shows success status
- **Paused for edit**: Rebase stopped at commit marked for editing, user makes changes then continues manually
- **Paused for conflict**: Rebase stopped at conflicting commit, output lists files needing resolution
- **Error**: No rebase started, output explains validation failure or invalid input

### Integration with Other Skills

This skill can be composed with:
- **git-historian**: For commit message generation and analysis
- **worktree-manager**: For isolated rebase operations in separate worktrees
- Any agent performing git history cleanup or commit organization
