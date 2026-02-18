# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with the Claude Code configuration module in this repository.

## Overview

This module configures Claude Code CLI through Nix home-manager, managing output styles, specialized agents, skills, workflow hooks, and MCP server integrations. The configuration is declarative and assets are copied to `~/.claude/` at system activation.

**Module location**: `system/modules/claude/`

**Key commands**:
```bash
# Rebuild to activate changes
nix develop .#building --command rebuild <hostname>

# Test agent directly
~/.claude/agents/<agent-name>.md

# Test skill directly
~/.claude/skills/<skill-name>/<skill-name>.sh

# View active configuration
cat ~/.claude/config.json
```

## Architecture & Patterns

### Module Structure

```
claude/
├── default.nix              # Home-manager configuration
├── output-styles/
│   └── bobert.md            # Third-person orchestrator output style
├── agents/                  # Specialized task agents
│   ├── agent-creator.md
│   ├── context-curator.md
│   ├── deep-researcher.md
│   ├── project-initiator.md
│   └── work-starter.md
└── skills/                  # Reusable command patterns
    ├── agent_reminder/      # UserPromptSubmit hook
    ├── create_memory/       # Org-roam node creation
    ├── read_memory/         # Org-roam node reading with Required Reading hook
    └── todo-writer/         # TODO generation from templates
```

### Core Patterns

1. **Declarative Configuration**: All Claude Code settings defined in `default.nix` via `programs.claude-code`
2. **Asset Installation**: Agents and skills directories copied to `~/.claude/` at activation
3. **YAML Frontmatter**: Agents use frontmatter for name, description, tools, skills, model selection
4. **Skill Definition**: Skills use SKILL.md with frontmatter + bash implementation scripts
5. **Hook System**: Bash scripts intercept workflow events (UserPromptSubmit, PostToolUse) with JSON I/O
6. **Environment Configuration**: `settings.env` passes ORG_ROAM_DIR to skills and hooks

### Output Styles

**Bobert** (`output-styles/bobert.md`):
- Third-person perspective meta-orchestrator ("Bobert will..." not "I will...")
- Five-phase methodology: Plan, Execute, Assert, Reflect, Share
- Read-only discipline: delegates all file modifications via Task tool
- Source-backed decision making with explicit citations
- Proactive context improvement recommendations

Activated via `settings.outputStyle = "Bobert"` in default.nix.

### Agents

Agents are specialized, atomic task handlers invoked via `/agent-name` or Task tool:

- **agent-maintainer** (opus): Agent design, creation, modification, and lifecycle management
- **context-curator**: Curates task-relevant context from org-roam memory dependencies
- **deep-researcher**: Conducts systematic research producing Learning Packets
- **project-initiator**: Automates project kickoff with exploration, TODOs, planning
- **work-starter** (sonnet): Transforms vague requests into structured TODO memories
- **code-monkey**: Fast code modifications and implementation work
- **git-historian**: Git repository analysis and archeology
- **skill-creator**: Designs and implements new Claude Code skills
- **adr-maintainer**: Architecture Decision Record creation and maintenance
- **technical-breakdown-maintainer**: Dynamic technical documentation management
- **todo-spec-memory-maintainer**: TODO list, specification, and memory maintenance

**Agent File Format**:
```yaml
---
name: agent-name
description: Brief description of when to use this agent
tools: Read, Write, Grep, Glob, Bash, Task, WebSearch, WebFetch
skills:
  - skill-name
model: sonnet|opus|haiku
---

# Agent prompt content in markdown...
```

### Skills

Skills are reusable command patterns invoked via `/skill-name`:

- **create_memory**: Creates org-roam nodes with UUID, timestamps, validated metadata
- **read_memory**: Reads org-roam nodes by UUID, triggers Required Reading hook
- **todo-writer**: Generates thorough TODOs following On Writing TODOs standards
- **agent_reminder**: Hook that promotes agent-first workflow on every prompt

**Skill File Format**:
```yaml
---
name: skill-name
description: Brief description
allowed-tools: Bash(~/.claude/skills/skill-name/*)
---

# Skill documentation and usage examples...
```

Implementation: Bash scripts in skill directory that read JSON from stdin.

### Hooks

Hooks intercept Claude Code workflow to inject automation:

**UserPromptSubmit** (`agent_reminder_hook.sh`):
- Triggers: Every user prompt submission
- Output: Reminder message promoting specialized agent usage
- Format: Stdout text appended to Claude's context

**PostToolUse** (`required_reading_hook.sh`):
- Triggers: After `read_memory` skill completes
- Parses: `* Required Reading` section from org-roam node
- Extracts: `[[id:UUID]]`, `[[file:path]]`, `[[jira:TICKET]]` links
- Output: Blocking JSON with mandatory loading instructions
- Format: `{"decision": "block", "reason": "..."}`

Hooks use bash + jq for JSON processing and must exit 0 for pass-through.

### MCP Servers

Configured in `settings.mcpServers`:
- **atlassian**: SSE connection to Atlassian MCP for Jira integration
- **github**: Docker-based stdio server for GitHub operations
- **org-roam**: (Commented out) Custom MCP for org-roam knowledge base access

Each server provides tools accessible via `mcp__<server>__<tool-name>` pattern.

### Team-Aware Agent Architecture

The agent ecosystem supports multi-agent collaboration through team workflows coordinated by Bobert (the orchestrator output style). This architecture enables parallel execution of independent work streams while maintaining coordination and integration quality.

**Team Collaboration Fundamentals**:

Agents operate in two modes:
1. **Individual Delegation**: Single agent handles complete task (default for single-dimensional work)
2. **Team Coordination**: Multiple agents work in parallel on independent dimensions (default for multi-dimensional work)

**Team Workflow Pattern**:
1. Bobert analyzes work for parallelization opportunities (team composition analysis)
2. Creates team via TeamCreate with descriptive name
3. Spawns teammates via TeamSpawn with context-rich prompts
4. Monitors progress via shared TaskList (TaskCreate, TaskUpdate)
5. Coordinates via Mailbox messaging (SendMessage tool)
6. Waits for ALL teammates to complete before proceeding
7. Integrates outputs and verifies coherence

**Agent Design for Team Collaboration**:

Agents designed for team workflows include:
- **SendMessage tool access**: Enables inter-agent coordination
- **Team Collaboration Awareness**: Understanding of mailbox communication patterns
- **Clear Scope Boundaries**: Well-defined responsibilities to minimize coordination overhead
- **Independent Operation**: Ability to work in parallel without blocking other teammates
- **Team Composition Recipes**: Documented collaboration patterns for common multi-agent scenarios

**Mailbox Communication**:

Agents use SendMessage tool for coordination:
- `type: "message"` - Direct message to specific teammate
- `type: "broadcast"` - Message to all teammates (use sparingly)
- Check mailbox periodically during work
- Respond to questions requiring guidance or scope clarification

**Task List Coordination**:

Shared task lists enable team progress tracking:
- TaskCreate: Create granular tasks for independent work units
- TaskUpdate: Update status (pending → in_progress → completed)
- TaskList: Monitor overall team progress
- Teams proceed to integration only when all tasks show status: completed

**Team Composition Decision Framework**:

Default presumption: Use agent teams unless individual delegation is clearly more appropriate.

Use teams when:
- Work decomposes into 2+ independent dimensions (research + implementation + documentation)
- Parallelization provides significant time savings (> 30% reduction vs sequential)
- Scope boundaries can be clearly drawn between agent responsibilities
- Integration complexity is manageable

Use individual delegation when:
- Single-dimensional work with no natural parallel decomposition
- Sequential dependencies prevent parallel execution
- Rapid turnaround (< 30 minutes) where team overhead exceeds value
- Integration complexity exceeds parallelization value

See `output-styles/bobert.md` for complete team coordination patterns and example workflows.

## Development Workflows

### Adding a New Agent

1. Create `agents/new-agent.md` with YAML frontmatter
2. Define tools, skills, and model in frontmatter
3. Write agent prompt with role definition, competencies, constraints
4. Rebuild system to copy to `~/.claude/agents/`
5. Test: `claude-code` and invoke via `/new-agent`

### Adding a New Skill

1. Create `skills/new-skill/` directory
2. Write `skills/new-skill/SKILL.md` with frontmatter and documentation
3. Implement `skills/new-skill/new-skill.sh` (read JSON from stdin, write to stdout)
4. Make script executable: `chmod +x new-skill.sh`
5. Rebuild system to copy to `~/.claude/skills/`
6. Test: `echo '{"arg":"value"}' | ~/.claude/skills/new-skill/new-skill.sh`

### Modifying Hooks

1. Edit hook script in `skills/<skill>/` directory
2. Ensure JSON output format matches hook type:
   - UserPromptSubmit: stdout text (no JSON)
   - PostToolUse: `{"decision": "pass"|"block", "reason": "..."}`
3. Test hook independently with sample JSON input
4. Rebuild system to update `~/.claude/skills/`
5. Verify: trigger the hook condition and check stderr logs

### Updating Configuration

1. Edit `default.nix` `programs.claude-code.settings`
2. Modify outputStyle, env vars, attribution, or hooks configuration
3. Rebuild system: `nix develop .#building --command rebuild <hostname>`
4. Verify: `cat ~/.claude/config.json` shows updated settings
5. Restart claude-code if needed to pick up changes

## Integration Points

### Org-roam Integration

- **Location**: `~/Library/Mobile Documents/com~apple~CloudDocs/notes/roam`
- **Access**: Via `ORG_ROAM_DIR` environment variable
- **Skills**: `create_memory`, `read_memory` interact with org-roam nodes
- **Format**: Org-mode files with `#+ROAM_*` properties and UUID-based filenames
- **Hook**: Required Reading automation parses `[[id:UUID]]` links

### Emacs Integration

Claude Code connects to Emacs via `claude-code-ide.el`:
- LSP integration via xref tools (find-definitions, find-references)
- Tree-sitter syntax analysis
- Project.el context awareness
- Automatic file/selection tracking

### Git Integration

Bobert output style commits during Reflect phase:
```bash
git commit --no-gpg-sign -m "descriptive message"
```

Note: `--no-gpg-sign` flag used to skip GPG signing in automated commits.

## Anti-Patterns

- **Don't** manually edit `~/.claude/*` files - changes will be overwritten on next rebuild
- **Don't** create agents without YAML frontmatter - they won't be recognized by Claude Code
- **Don't** skip the Required Reading hook - it ensures context completeness
- **Don't** use blocking decisions in UserPromptSubmit hooks - they create friction (use informational messages)
- **Don't** create skills that modify state without clear documentation - skills should be predictable
- **Don't** add MCP servers without authentication setup - many require tokens/credentials

## Debugging

### Agent Not Found
- Check `~/.claude/agents/<agent>.md` exists after rebuild
- Verify YAML frontmatter is valid (use yamllint)
- Check claude-code logs: `~/.claude/logs/`

### Skill Fails Silently
- Test script directly: `echo '{}' | ~/.claude/skills/<skill>/<skill>.sh`
- Check script is executable: `ls -l ~/.claude/skills/<skill>/<skill>.sh`
- Review stderr output from hook execution

### Hook Not Triggering
- Verify hook is configured in `default.nix` settings.hooks
- Check hook script returns exit code 0 for pass-through
- Look for syntax errors: `bash -n ~/.claude/skills/<skill>/<hook>.sh`
- Examine claude-code stderr for hook error messages

### Required Reading Loop
- Hook checks for UUID cycles and tells Claude to skip already-loaded nodes
- If stuck in loop, the memory graph may have circular dependencies
- Review `* Required Reading` sections in org-roam nodes for circular references
