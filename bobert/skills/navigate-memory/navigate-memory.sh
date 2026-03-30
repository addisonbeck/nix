#!/usr/bin/env bash
# navigate-memory.sh — org-roam query helper for the navigate-memory skill.
# Detects execution context via BOBERT_EMACS and routes to batch or daemon path.
# Usage: navigate-memory.sh '<elisp-expression>'
# All queries are read-only. Never call org-roam-db-update-file or write files.

set -euo pipefail

QUERY="${1:-}"
if [ -z "$QUERY" ]; then
  echo "Usage: navigate-memory.sh '<elisp-expression>'" >&2
  exit 1
fi

SOCKET="$HOME/.emacs.d/server/server"

if [ -n "${BOBERT_EMACS:-}" ]; then
  # Batch mode path (running inside bobert-with-emacs)
  # BOBERT_EMACS is set to the Nix store path of the bundled emacs-nox binary.
  # BOBERT_ORG_ROAM_DB is the isolated database at ~/.bobert/org-roam.db.
  DB="${BOBERT_ORG_ROAM_DB:-$HOME/.bobert/org-roam.db}"

  if [ ! -f "$DB" ]; then
    echo "Error: org-roam database not found at $DB. Has bobert-with-emacs run db-sync?" >&2
    exit 1
  fi

  "$BOBERT_EMACS" -Q --batch --eval \
    "(progn \
       (require 'org-roam) \
       (setq org-roam-directory \"${ORG_ROAM_DIR:-}\") \
       (setq org-roam-db-location \"$DB\") \
       $QUERY)"
else
  # Daemon mode path (running inside standard bobert)
  # BOBERT_EMACS is not set — use the running host Emacs daemon via emacsclient.
  # The daemon uses a non-default socket path; always specify --socket-name.
  EMACSCLIENT="${HOME}/.nix-profile/bin/emacsclient"

  if ! "$EMACSCLIENT" --socket-name="$SOCKET" --eval "(featurep 'org-roam)" > /dev/null 2>&1; then
    echo "Error: Emacs daemon not running or org-roam not loaded. Check socket at $SOCKET." >&2
    exit 1
  fi

  "$EMACSCLIENT" --socket-name="$SOCKET" --eval "$QUERY"
fi
