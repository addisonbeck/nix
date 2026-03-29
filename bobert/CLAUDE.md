# CLAUDE.md — bobert/

This file provides guidance to Claude Code when working inside the `bobert/` directory.

## Overview

This is the standalone Nix flake that manages Claude Code configuration for the `air`/`bw` macOS systems. It owns agents, skills, hooks, output-styles, and settings. The `bobert` wrapper syncs all of this to `~/.claude/` and execs `claude` on invocation.

**No system rebuild is needed** to pick up changes here. Edits to agents, skills, or hooks take effect on the next `nix run /Users/me/nix/bobert` invocation.

The exception is `flake.nix` itself — changes to settings, the wrapper script, or the `bobertData` derivation require Nix to re-evaluate the flake (still no rebuild, but `nix run` will re-build the derivation).

## Structure

```
bobert/
├── flake.nix          # Settings attrset, bobertData derivation, bobert wrapper
├── agents/            # Agent definition files (*.md)
├── skills/            # Skill directories (SKILL.md + *.sh)
├── hooks/             # Hook scripts (*.sh)
└── output-styles/     # Output style definitions (*.md)
    └── bobert.md
```

## Modifying Settings

All Claude Code settings are in `flake.nix` under the `settings` let-binding. This maps 1:1 to `~/.claude/settings.json` via `builtins.toJSON`.

Key fields:
- `outputStyle` — which output style Claude loads
- `env` — env vars injected into every session
- `hooks` — event handler configuration (SessionStart, PreToolUse, PostToolUse, UserPromptSubmit)
- `mcpServers` — MCP server definitions
- `spinnerVerbs` — custom loading messages

After editing `flake.nix`, the new settings take effect on the next `nix run`.

## Adding an Agent

1. Create `agents/<agent-name>.md`
2. Add YAML frontmatter with `name`, `description`, `tools`, and optionally `skills` and `model`
3. Write the agent prompt body in markdown below the frontmatter

```yaml
---
name: my-agent
description: One-line description of when to invoke this agent
tools: Read, Write, Grep, Glob, Bash
model: sonnet
---
```

Agents are synced to `~/.claude/agents/` via rsync. The existing set is deleted and replaced on each run.

## Adding a Skill

1. Create `skills/<skill-name>/` directory
2. Write `skills/<skill-name>/SKILL.md` with YAML frontmatter and documentation
3. Write `skills/<skill-name>/<skill-name>.sh` — bash script, reads context JSON from stdin
4. Make the script executable: `chmod +x skills/<skill-name>/<skill-name>.sh`

The bobert wrapper runs `find ... -name "*.sh" -exec chmod +x` so executable bits are restored even if lost.

## Adding a Hook

1. Write `hooks/<hook-name>.sh`
2. Register it in `flake.nix` under `settings.hooks.<EventName>[].hooks[]`
3. Hook scripts receive JSON context on stdin and write JSON to stdout:
   - Pass-through: exit 0 (no output required for most hook types)
   - Block (PreToolUse/PostToolUse only): `{"decision": "block", "reason": "..."}`

## Anti-Patterns

- **Don't** edit `~/.claude/` directly — the bobert wrapper overwrites it with rsync on next run
- **Don't** add `pkgs.claude-code-acp` to this flake's `runtimeInputs` — it's installed via the system flake's overlay at the pinned version; the wrapper execs `claude` from PATH
- **Don't** reference `self` in the flake outputs — causes infinite recursion when interpolating `${bobert}/bin/bobert` in the apps output
- **Don't** add `home-manager` as a flake input — this flake is intentionally standalone
