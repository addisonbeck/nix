{
  lib,
  pkgs,
  ...
}: {
  launchd.agents = lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    bobert-inbox-loop = {
      enable = true;
      config = {
        Label = "bobert-inbox-loop";
        Program = "${pkgs.writeShellScript "inbox-loop" ''
          set -euo pipefail

          INBOX="/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/agenda/bobert-work-inbox.org"
          AGENDA="/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/agenda/bobert-work-agenda.org"
          LOG_DIR="$HOME/.local/log/inbox-loop"
          LOG_FILE="$LOG_DIR/$(date +%Y-%m-%d).log"

          mkdir -p "$LOG_DIR"

          echo "[$(date -Iseconds)] inbox-loop: starting" >> "$LOG_FILE"

          # Pre-check: skip if inbox file does not exist or has no unprocessed headings
          if [ ! -f "$INBOX" ]; then
            echo "[$(date -Iseconds)] inbox-loop: inbox file not found, skipping" >> "$LOG_FILE"
            exit 0
          fi

          # Check for unprocessed level-2 headings (** ...) that lack :PROCESSED: t in their properties drawer.
          # grep-based approaches fail because heading lines never contain :PROCESSED: t themselves --
          # the marker lives on a separate line inside the :PROPERTIES: drawer. awk tracks per-item state.
          if awk '
            /^\*\* / {
              if (in_item && !processed) { exit 1 }
              in_item = 1; processed = 0
            }
            /^:PROCESSED: t/ { processed = 1 }
            END { if (in_item && !processed) { exit 1 } }
          ' "$INBOX"; then
            echo "[$(date -Iseconds)] inbox-loop: no unprocessed items, skipping" >> "$LOG_FILE"
            exit 0
          fi

          echo "[$(date -Iseconds)] inbox-loop: invoking claude inbox-processor" >> "$LOG_FILE"

          nix run /Users/me/nix/bobert -- --print \
            --dangerously-skip-permissions \
            --agent inbox-processor \
            -p "Process unprocessed inbox items from $INBOX and append structured TODOs to $AGENDA" \
            2>> "$LOG_FILE"

          echo "[$(date -Iseconds)] inbox-loop: done" >> "$LOG_FILE"
        ''}";
        ProcessType = "Background";
        RunAtLoad = false;
        StartInterval = 300;
        EnvironmentVariables = {
          BOBERT_INBOX = "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/agenda/bobert-work-inbox.org";
          BOBERT_AGENDA = "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/agenda/bobert-work-agenda.org";
          HOME = "/Users/me";
          PATH = "/Users/me/.nix-profile/bin:/run/current-system/sw/bin:/usr/local/bin:/usr/bin:/bin";
        };
        StandardOutPath = "/Users/me/.local/log/inbox-loop/launchd-stdout.log";
        StandardErrorPath = "/Users/me/.local/log/inbox-loop/launchd-stderr.log";
      };
    };
  };
}
