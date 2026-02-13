---
name: read_memory
description: |
  Fetches an org-roam memory node by its UUID. Returns the node's title,
  content, and file path. Use when the user asks you to read a memory node
  or when you need to access specific org-roam knowledge by ID.
allowed-tools: Bash(~/.claude/skills/read_memory/*)
---

# read_memory Skill

When invoked, execute the bundled read_memory.sh script with the UUID argument:

```bash
~/.claude/skills/read_memory/read_memory.sh $ARGUMENTS
```

This script searches the org-roam directory for a node with the given UUID and returns JSON containing:
- **id**: Node UUID
- **title**: Node title from #+TITLE directive  
- **content**: Full node content in org-mode format
- **file**: Absolute path to the .org file

The script handles errors gracefully - if the node isn't found, it returns an error message.

## Example usage
```
/read_memory FF665E5D-6093-4830-ADB7-48CAE2FA65D0
```
