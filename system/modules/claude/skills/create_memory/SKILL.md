---
name: create_memory
description: |
  Creates a new org-roam memory node with validated metadata and proper
  org-mode structure. Generates UUID, timestamps, and file naming automatically.
  Use when creating new memory nodes, documentation, or knowledge entries.
allowed-tools: Bash(~/.claude/skills/create_memory/*)
---

# create_memory Skill

When invoked, pipe JSON input to the bundled create_memory.sh script:

```bash
echo '$ARGUMENTS' | ~/.claude/skills/create_memory/create_memory.sh
```

The JSON input must contain:
- **title** (string): Node title (1-200 characters)
- **memory_type** (string): One of: episodic, semantic, procedural, associative, working, reflective, reference (validated)
- **tags** (array): At least one tag string
- **aliases** (array): At least one alias string
- **content** (string): Node content in org-mode markup
- **subfolder** (string, optional): Subdirectory within ORG_ROAM_DIR for file creation (e.g., "adr", "todo", "technical-breakdown"). Defaults to root directory if not provided.

The script validates that memory_type is one of the allowed values and returns an error if invalid.

The script generates a complete org-roam node with:
- Auto-generated UUID
- Timestamp in PROPERTIES drawer
- Slugified filename from title
- Proper org-mode structure (PROPERTIES, #+TITLE, #+FILETAGS)

Returns JSON with the generated node ID, file path, and title.

## Example usage

**Basic usage (root directory)**:
```
/create_memory {"title":"Implementation Notes","memory_type":"procedural","tags":["development"],"aliases":["impl notes"],"content":"* Overview\n\nImplementation details..."}
```

**With subfolder parameter**:
```
/create_memory {"title":"Decision Record 1","memory_type":"semantic","tags":["adr","architecture"],"aliases":["ADR-001"],"content":"* Context\n\nArchitecture decision...","subfolder":"adr"}
```

This creates the file at `$ORG_ROAM_DIR/adr/2026-03-06-14-45-30.decision-record-1.org` instead of the root directory.

**Note**: The caller is responsible for ensuring the subfolder directory exists. The script does not create directories automatically.
