#!/usr/bin/env bash
# Hook that informs Claude about the ORG_ROAM_DIR environment variable.
# Registered on SessionStart and SubagentStart so each session sees it exactly once.

echo "\$ORG_ROAM_DIR is the memory directory. Its current value is: ${ORG_ROAM_DIR:-<not set>}"
