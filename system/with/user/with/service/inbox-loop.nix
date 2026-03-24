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

          # Pre-check: skip if inbox file does not exist or has no TODO headings
          if [ ! -f "$INBOX" ] || ! grep -q "^\* TODO" "$INBOX" 2>/dev/null; then
            echo "[$(date -Iseconds)] inbox-loop: no unprocessed items, skipping" >> "$LOG_FILE"
            exit 0
          fi

          echo "[$(date -Iseconds)] inbox-loop: invoking claude inbox-processor" >> "$LOG_FILE"

          claude --print \
            --dangerously-skip-permissions \
            --allowedTools "Read,Write,Bash" \
            --model claude-haiku-4-5-20251001 \
            -p "Process the inbox: read $INBOX for unprocessed TODO items and append them to $AGENDA" \
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
          PATH = "/run/current-system/sw/bin:/usr/local/bin:/usr/bin:/bin";
        };
        StandardOutPath = "/Users/me/.local/log/inbox-loop/launchd-stdout.log";
        StandardErrorPath = "/Users/me/.local/log/inbox-loop/launchd-stderr.log";
      };
    };
  };
}
