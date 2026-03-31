---
name: run-arbitrary-elisp
description: |
  Teaches how to execute arbitrary Elisp against the running Emacs daemon via emacsclient.
  Use when you need to trigger Emacs actions from a shell: open files, start captures,
  run magit, query buffer state, or drive any Emacs function without opening a frame.
---

# run-arbitrary-elisp Skill

This skill teaches you how to send arbitrary Elisp expressions to the user's running Emacs daemon using `emacsclient`. The daemon is managed by launchd and exposes a Unix socket that `emacsclient` connects to for evaluation.

When invoked, use the patterns below to construct and run `emacsclient` commands via the Bash tool. No helper script is involved — the skill is entirely instruction-based.

## When to Use

Use this skill when you need to:
- Open a file in Emacs (e.g., a file an agent just created or modified)
- Trigger an org-capture with a specific template key
- Open the org agenda or a project's magit status
- Query Emacs state (active buffers, project paths, variable values)
- Drive any interactive Emacs workflow from a shell command

## When NOT to Use

- If Emacs is not running and the task does not require it — just edit files directly with Write/Edit tools
- If the expression is destructive and you have not confirmed the daemon is healthy

## Environment Dependencies

- **Socket path**: `~/.emacs.d/server/server` — hardcoded for this user's configuration
- **launchd label**: `org.gnu.emacs.daemon`
- **Required tool**: `emacsclient` (available system-wide via the Nix-managed Emacs install)
- **Platform**: macOS only (launchd-specific restart command)

## Base Invocation Pattern

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e '(your-elisp-here)'
```

Always use `--socket-name` with the explicit socket path. Do not rely on the default socket resolution — the user's `ec` and `em` wrappers both set this explicitly.

## Daemon Health Check

Before sending expressions, verify the daemon is responsive:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e "(+ 1 1)" >/dev/null 2>&1
```

Exit code 0 means the daemon is running and accepting connections. Any non-zero exit means the daemon is unreachable.

## Daemon Restart

If the health check fails, restart via launchctl:

```bash
/bin/launchctl kickstart -k gui/$(id -u)/org.gnu.emacs.daemon
sleep 2
```

The `-k` flag kills the existing service before starting fresh. The `sleep 2` gives the daemon time to initialize and open its socket before the next `emacsclient` call.

**Full pattern — health check then restart if needed:**

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e "(+ 1 1)" >/dev/null 2>&1 || {
  /bin/launchctl kickstart -k gui/$(id -u)/org.gnu.emacs.daemon
  sleep 2
}
```

## Output Handling

### Capture return value

When you need the result of an expression in a shell variable:

```bash
result=$(emacsclient --socket-name="$HOME/.emacs.d/server/server" -e '(buffer-name)')
echo "$result"
```

The return value is printed to stdout as a Lisp-printed form (strings include surrounding quotes).

### Suppress output (side-effects only)

When you only care that something happens and not what it returns, use `-u` to suppress output:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -u -e '(org-agenda nil "a")'
```

### Non-blocking (fire and forget)

Use `-n` when you do not want to wait for Emacs to finish processing the expression:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(magit-status "/Users/me/nix")'
```

This returns immediately. Useful when opening interactive UI (magit, agenda) that the user will drive — there is no meaningful return value to wait for.

## Shell Quoting Guidance

The expression is passed as a shell argument to `emacsclient -e`. Quoting rules:

**Simple expressions** — use single quotes around the Elisp:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e '(find-file "~/notes/inbox.org")'
```

**Expressions containing shell variables** — use double quotes and escape the inner Elisp strings:

```bash
filepath="/Users/me/nix/bobert/agents/my-agent.md"
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e "(find-file \"$filepath\")"
```

**Complex expressions with both** — assign the Elisp to a variable first to avoid quoting confusion:

```bash
filepath="/Users/me/nix/bobert/skills/run-arbitrary-elisp/SKILL.md"
expr="(find-file \"$filepath\")"
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e "$expr"
```

**Expressions with backticks or special characters** — use `printf` to build the expression:

```bash
expr=$(printf '(message "Path is: %s")' "$some_var")
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e "$expr"
```

## Common Use Cases

### Open a specific file in a buffer

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(find-file "/Users/me/nix/bobert/flake.nix")'
```

With a shell variable for the path:

```bash
filepath="/Users/me/nix/bobert/flake.nix"
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e "(find-file \"$filepath\")"
```

### Open magit for a specific repository

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(magit-status "/Users/me/nix")'
```

For the notes repo:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(magit-status "/Users/me/binwarden/clients")'
```

### Open a project by name (using my/find-project)

The user has named projects in `my/projects`: `"nix"`, `"notes"`, `"clients"`, `"binwarden"`.

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(my/find-project "nix")'
```

### Start an org-capture with a specific template

Available template keys: `"l"` (log), `"e"` (event), `"t"` (todo with deadline), `"a"` (link), `"s"` (source block), `"b"` (budgeting submenu).

```bash
# Open capture with the todo template
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(org-capture nil "t")'

# Open capture with the log template
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(org-capture nil "l")'
```

### Open the org agenda

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(org-agenda nil "a")'
```

### Open the org-roam node finder

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(org-roam-node-find)'
```

### Open an org-roam node by UUID

```bash
uuid="FF665E5D-6093-4830-ADB7-48CAE2FA65D0"
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e \
  "(org-roam-node-visit (org-roam-node-from-id \"$uuid\"))"
```

### Raise or focus the Emacs frame (bring to foreground)

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(select-frame-set-input-focus (selected-frame))'
```

If no frame exists yet and you want to open one:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -c -n -e '(select-frame-set-input-focus (selected-frame))'
```

### Capture return value from an expression

```bash
# Get the current buffer name
result=$(emacsclient --socket-name="$HOME/.emacs.d/server/server" -e '(buffer-name)')
echo "Current buffer: $result"

# Get a variable's value
roam_dir=$(emacsclient --socket-name="$HOME/.emacs.d/server/server" -e 'org-roam-directory')
echo "Roam dir: $roam_dir"
```

Note: String values are returned wrapped in double quotes (Lisp print format). Strip them with `tr -d '"'` if you need a bare shell string:

```bash
roam_dir=$(emacsclient --socket-name="$HOME/.emacs.d/server/server" -e 'org-roam-directory' | tr -d '"')
```

## Error Handling

`emacsclient` exits non-zero when:
- The socket does not exist or the daemon is not running (exit 1)
- The Elisp expression signals an error (exit 1, error printed to stderr)
- The socket exists but no daemon is listening (connection refused)

**Recommended pattern for scripts where Emacs availability is optional:**

```bash
SOCKET="$HOME/.emacs.d/server/server"
if emacsclient --socket-name="$SOCKET" -e "(+ 1 1)" >/dev/null 2>&1; then
  emacsclient --socket-name="$SOCKET" -n -e '(find-file "/path/to/file")'
else
  echo "Emacs daemon not running — skipping frame open"
fi
```

**Pattern for scripts where Emacs is required (restart if down):**

```bash
SOCKET="$HOME/.emacs.d/server/server"
emacsclient --socket-name="$SOCKET" -e "(+ 1 1)" >/dev/null 2>&1 || {
  /bin/launchctl kickstart -k gui/$(id -u)/org.gnu.emacs.daemon
  sleep 2
}
emacsclient --socket-name="$SOCKET" -n -e '(org-agenda nil "a")'
```

## Integration Patterns

### Open a file an agent just created

After writing a new file, open it in Emacs so the user sees it immediately:

```bash
newfile="/Users/me/nix/bobert/skills/my-skill/SKILL.md"
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e "(find-file \"$newfile\")"
```

### Trigger org-roam DB sync after creating new nodes

After `create_memory` writes new `.org` files, sync the org-roam database:

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -u -e '(org-roam-db-sync)'
```

### Invoke from a PostToolUse hook

Hooks run as bash scripts. Use the health-check-then-act pattern to safely drive Emacs from hooks without crashing the hook if Emacs is down:

```bash
#!/usr/bin/env bash
SOCKET="$HOME/.emacs.d/server/server"
emacsclient --socket-name="$SOCKET" -e "(+ 1 1)" >/dev/null 2>&1 || exit 0
emacsclient --socket-name="$SOCKET" -n -u -e '(dashboard-refresh-buffer)'
```

### Evaluate an expression and use its output in an agent decision

```bash
# Check if a specific buffer is open
result=$(emacsclient --socket-name="$HOME/.emacs.d/server/server" \
  -e '(if (get-buffer "inbox.org") "open" "closed")' 2>/dev/null)
# result will be "open" or "closed" (with Lisp string quotes)
```

## Usage & Testing Guidance

### Verify daemon is running

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e "(+ 1 1)"
# Expected output: 2
```

### Test opening a file

```bash
emacsclient --socket-name="$HOME/.emacs.d/server/server" -n -e '(find-file "/Users/me/nix/CLAUDE.md")'
# Expected: no output, file opens in Emacs
```

### Test error case (daemon down)

```bash
# Stop the daemon first (careful — this kills your session)
# /bin/launchctl kill SIGTERM gui/$(id -u)/org.gnu.emacs.daemon
emacsclient --socket-name="$HOME/.emacs.d/server/server" -e "(+ 1 1)"
# Expected: non-zero exit, error message about socket/connection
```

### Claude Code Invocation

```
/run-arbitrary-elisp
```

This loads the skill instructions into context. Then use the Bash tool with the patterns above to send Elisp to the running Emacs daemon.

### Installation

```bash
nix run /Users/me/nix/bobert
ls ~/.claude/skills/run-arbitrary-elisp/
```
