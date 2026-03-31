# Bobert

> Emotion equals X over Y plus a square root of a hypotenuse
> -- Bobert 6B

<img width="300" height="168" alt="image" src="https://github.com/user-attachments/assets/180df460-b6de-4e97-8c32-be546cedd045" />

## Overview

Bobert is a standalone Nix flake that wraps [Claude Code](https://claude.ai/code) with a managed configuration: agents, skills, hooks, output styles, and settings — all version-controlled and synced to `~/.claude/` on every invocation.


## Usage

### Packages

There are three runnable packages:

| Package | Command | Purpose |
|---------|---------|---------|
| `bobert` (default) | `nix run` | Standard Claude Code wrapper |
| `bobert-with-emacs` | `nix run .#bobert-with-emacs` | Claude + Emacs/org-roam for DB-backed memory queries |
| `bobert-view` | `nix run .#bobert-view` | Serves org-roam-ui-lite graph UI |

### Local

```bash
# Default (bobert)
nix run /Users/me/nix/bobert

# With Claude arguments
nix run /Users/me/nix/bobert -- --resume

# With Emacs and org-roam DB support
nix run /Users/me/nix/bobert#bobert-with-emacs

# Serve the org-roam graph viewer
nix run /Users/me/nix/bobert#bobert-view

# Via shell aliases (after system rebuild)
bobert
claude
```

### Remote

```bash
# Default (bobert)
nix run github:addisonbeck/nix?dir=bobert

# With Claude arguments
nix run github:addisonbeck/nix?dir=bobert -- --resume

# Serve the org-roam graph viewer
nix run github:addisonbeck/nix?dir=bobert#bobert-view
```

> **Note**: `bobert-with-emacs` cannot be run remotely — its `org-roam-ui-lite` input references a local path.

### bobert-with-emacs

Uses `~/.bobert/` as its config dir (instead of `~/.claude/`) and sets `CLAUDE_CONFIG_DIR` accordingly. It bundles Emacs with `org-roam` and `org-roam-ql` so the `navigate-memory` skill can query the org-roam database without a running Emacs daemon.

On first run, if no database exists at `~/.emacs.d/org-roam.db`, it syncs one to `~/.bobert/org-roam.db` from `ORG_ROAM_DIR`.

### bobert-view

Serves the org-roam knowledge graph on localhost via `org-roam-ui-lite`. Reads the database from `$BOBERT_ORG_ROAM_DB` or falls back to `~/.emacs.d/org-roam.db`.

```bash
# Point at a specific DB
BOBERT_ORG_ROAM_DB=~/.bobert/org-roam.db nix run /Users/me/nix/bobert#bobert-view
```

On each invocation, Bobert:
1. Rsyncs `agents/`, `skills/`, `hooks/`, `output-styles/` into `~/.bobert/`
2. Writes `~/.bobert/settings.json` if content has changed
3. Exec-replaces itself with `claude "$@"`

---

## Structure

```
bobert/
├── flake.nix          # Settings, wrapper script, Nix package definition
├── agents/            # Specialized task agents (*.md with YAML frontmatter)
├── skills/            # Reusable command patterns (SKILL.md + *.sh)
├── hooks/             # Workflow hook scripts (bash, JSON I/O)
└── output-styles/     # Claude output style definitions
    └── bobert.md      # Third-person orchestrator persona
```

## Configuration

All Claude Code settings live in `flake.nix` under the `settings` attrset:

- **`outputStyle`** — active output style (`"Bobert"`)
- **`env`** — environment variables injected into every session (ORG_ROAM_DIR, etc.)
- **`hooks`** — SessionStart, UserPromptSubmit, PreToolUse, PostToolUse handlers
- **`mcpServers`** — MCP server definitions (Atlassian, GitHub)

Changes to `flake.nix` take effect on the next `nix run` — no system rebuild needed.

## Adding an Agent

```bash
# Create agent file
cat > bobert/agents/my-agent.md << 'EOF'
---
name: my-agent
description: What this agent does and when to use it
tools: Read, Write, Grep, Glob, Bash
model: sonnet
---

Agent prompt content here...
EOF

# Active on next invocation
nix run /Users/me/nix/bobert
```

## Adding a Skill

```bash
mkdir bobert/skills/my-skill
# Write bobert/skills/my-skill/SKILL.md (frontmatter + docs)
# Write bobert/skills/my-skill/my-skill.sh (bash, reads JSON from stdin)
chmod +x bobert/skills/my-skill/my-skill.sh
```

## Integrations

| Integration | How |
|-------------|-----|
| Org-roam | `ORG_ROAM_DIR` env var in `settings.env` |
| Atlassian / Jira | SSE MCP server in `settings.mcpServers` |
| GitHub | Docker stdio MCP server in `settings.mcpServers` |
| Required Reading | `PostToolUse` hook on `read_memory` skill |
