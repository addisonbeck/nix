# Bobert

> Emotion equals X over Y plus a square root of a hypotenuse
> -- Bobert 6B

<img width="300" height="168" alt="image" src="https://github.com/user-attachments/assets/180df460-b6de-4e97-8c32-be546cedd045" />

## Overview

Bobert is a standalone Nix flake that wraps [Claude Code](https://claude.ai/code) with a managed configuration: agents, skills, hooks, output styles, and settings — all version-controlled and synced to `~/.claude/` on every invocation.


## Usage

```bash
# Run directly
nix run /Users/me/nix/bobert

# With arguments
nix run /Users/me/nix/bobert -- --resume

# Via shell aliases (after system rebuild)
bobert
claude
```

On each invocation, Bobert:
1. Rsyncs `agents/`, `skills/`, `hooks/`, `output-styles/` into `~/.claude/`
2. Writes `~/.claude/settings.json` if content has changed
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
