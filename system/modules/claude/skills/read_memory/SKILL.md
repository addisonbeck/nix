---
name: read_memory
description: |
  Fetches an org-roam memory node by its UUID. Returns the node's title,
  content, and file path. This skill returns ONLY the requested node - it 
  does NOT automatically traverse Required Reading links. The PostToolUse hook
  provides context about linked resources, and Claude decides which tools to use.
---

# read_memory Skill

## Purpose
Provides direct access to org-roam memory nodes stored in Addison's
knowledge base.

## Arguments
- **id** (required): The org-roam node UUID in uppercase format
  (e.g., "FF665E5D-6093-4830-ADB7-48CAE2FA65D0")

## Returns
JSON object containing:
- **id**: Node UUID
- **title**: Node title from #+TITLE directive
- **content**: Full node content in org-mode format
- **file**: Absolute path to the .org file

## Example
```
/read_memory FF665E5D-6093-4830-ADB7-48CAE2FA65D0
```
