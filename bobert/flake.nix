{
  description = "Bobert Claude Code configuration wrapper";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    org-roam-ui-lite = {
      url = "git+file:///Users/me/binwarden/addisonbeck-org-roam-ui-lite/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    org-roam-ui-lite,
    ...
  }: let
    supportedSystems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux" "aarch64-linux"];
    forAllSystems = fn: nixpkgs.lib.genAttrs supportedSystems fn;

    perSystem = system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      settings = {
        outputStyle = "Bobert";

        env = {
          ORG_ROAM_DIR = "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/roam";
          CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
          BOBERT_INBOX = "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/agenda/bobert-work-inbox.org";
          BOBERT_AGENDA = "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/agenda/bobert-work-agenda.org";
        };

        attribution = {
          commit = "";
          pr = "";
        };

        includeGitInstructions = false;
        prefersReducedMotion = true;

        spinnerVerbs = {
          mode = "replace";
          verbs = [
            "Predicting the next word"
            "Guessing"
            "Hallucinating"
            "Pooping tokens"
            "Pretending to understand"
            "Making shit up"
            "Don't forget to take a break"
          ];
        };

        hooks = {
          SessionStart = [
            {
              hooks = [
                {
                  type = "command";
                  command = "~/.claude/hooks/org_roam_env_hook.sh";
                  statusMessage = "Injecting ORG_ROAM_DIR context...";
                }
              ];
            }
          ];

          SubagentStart = [
            {
              hooks = [
                {
                  type = "command";
                  command = "~/.claude/hooks/org_roam_env_hook.sh";
                  statusMessage = "Injecting ORG_ROAM_DIR context...";
                }
              ];
            }
          ];

          UserPromptSubmit = [
            {
              hooks = [
                {
                  type = "command";
                  command = "~/.claude/hooks/agent_reminder_hook.sh";
                  statusMessage = "Checking agent workflow...";
                }
              ];
            }
          ];

          PostToolUse = [
            {
              matcher = "Bash";
              hooks = [
                {
                  type = "command";
                  command = "~/.claude/skills/read_memory/required_reading_hook.sh";
                  statusMessage = "Processing Required Reading...";
                }
              ];
            }
          ];

          PreToolUse = [
            {
              matcher = "Bash";
              hooks = [
                {
                  type = "command";
                  command = "~/.claude/hooks/git_commit_gate_hook.sh";
                  statusMessage = "Checking git commit gate...";
                }
                {
                  type = "command";
                  command = "~/.claude/hooks/org_roam_bash_protection_hook.sh";
                  statusMessage = "Checking org-roam Bash protection...";
                }
              ];
            }
            {
              matcher = "Write";
              hooks = [
                {
                  type = "command";
                  command = "~/.claude/hooks/org_roam_protection_hook.sh";
                  statusMessage = "Checking org-roam protection...";
                }
              ];
            }
            {
              matcher = "Read";
              hooks = [
                {
                  type = "command";
                  command = "~/.claude/hooks/project_memory_protection_hook.sh";
                  statusMessage = "Checking memory access pattern...";
                }
              ];
            }
          ];
        };

        mcpServers = {
          atlassian = {
            type = "sse";
            url = "https://mcp.atlassian.com/v1/sse";
          };

          github = {
            command = "docker";
            type = "stdio";
            args = [
              "run"
              "-i"
              "--rm"
              "-e"
              "GITHUB_PERSONAL_ACCESS_TOKEN"
              "ghcr.io/github/github-mcp-server"
            ];
          };
        };
      };

      settingsFile = pkgs.writeText "claude-settings.json" (builtins.toJSON settings);

      bobertData = pkgs.runCommand "bobert-data" {} ''
        mkdir -p $out/agents $out/skills $out/hooks $out/output-styles
        cp -r ${./agents}/. $out/agents/
        cp -r ${./skills}/. $out/skills/
        cp -r ${./hooks}/. $out/hooks/
        cp -r ${./output-styles}/. $out/output-styles/
        cp ${settingsFile} $out/settings.json
      '';

      emacsNoxNoMail = pkgs.emacs-nox.override {withMailutils = false;};

      bobertEmacs = (pkgs.emacsPackagesFor emacsNoxNoMail).emacsWithPackages (epkgs: [
        epkgs.org-roam
        epkgs.org-roam-ql
      ]);

      bobert-with-emacs = pkgs.writeShellApplication {
        name = "bobert-with-emacs";
        runtimeInputs = [pkgs.rsync];
        text = ''
          CLAUDE_DIR="$HOME/.bobert"
          export CLAUDE_CONFIG_DIR="$HOME/.bobert"
          export BOBERT_EMACS="${bobertEmacs}/bin/emacs"
          DATA="${bobertData}"

          for subdir in agents skills hooks output-styles; do
            target="$CLAUDE_DIR/$subdir"
            [ -e "$target" ] && [ ! -d "$target" ] && rm -f "$target"
            mkdir -p "$target"
          done

          rsync --archive --delete "$DATA/agents/" "$CLAUDE_DIR/agents/"
          rsync --archive --delete "$DATA/skills/" "$CLAUDE_DIR/skills/"
          rsync --archive --delete "$DATA/hooks/" "$CLAUDE_DIR/hooks/"
          rsync --archive --delete "$DATA/output-styles/" "$CLAUDE_DIR/output-styles/"

          SETTINGS_DEST="$CLAUDE_DIR/settings.json"
          if ! diff -q "$DATA/settings.json" "$SETTINGS_DEST" > /dev/null 2>&1; then
            rm -f "$SETTINGS_DEST"
            cp "$DATA/settings.json" "$SETTINGS_DEST"
          fi

          find "$CLAUDE_DIR/hooks" "$CLAUDE_DIR/skills" -name "*.sh" -exec chmod +x {} \;

          # Use the host DB if it exists (always current, no sync needed).
          # Fall back to the bobert-specific DB, creating it via sync if absent.
          if [ -z "''${BOBERT_ORG_ROAM_DB:-}" ]; then
            if [ -f "$HOME/.emacs.d/org-roam.db" ]; then
              export BOBERT_ORG_ROAM_DB="$HOME/.emacs.d/org-roam.db"
            else
              export BOBERT_ORG_ROAM_DB="$HOME/.bobert/org-roam.db"
            fi
          fi

          if [ ! -f "$BOBERT_ORG_ROAM_DB" ]; then
            echo "bobert-with-emacs: no org-roam database found, syncing from ${settings.env.ORG_ROAM_DIR}..." >&2
            mkdir -p "$(dirname "$BOBERT_ORG_ROAM_DB")"
            "$BOBERT_EMACS" -Q --batch --eval \
              "(progn \
                 (require 'org-roam) \
                 (setq org-roam-directory \"${settings.env.ORG_ROAM_DIR}\") \
                 (setq org-roam-db-location \"$BOBERT_ORG_ROAM_DB\") \
                 (org-roam-db-sync))"
          fi

          exec ${pkgs.claude-code}/bin/claude "$@"
        '';
      };

      # Upstream serve script hardcodes serve.js but the bundle ships serve.mjs.
      # The frontend also uses baseUrl:"./" which some browsers resolve as /.api/…
      # instead of /api/…, causing 404s. Patch both issues at build time.
      patchedBundle = pkgs.runCommand "org-roam-ui-lite-bundle-patched" {} ''
        origServe="${org-roam-ui-lite.packages.${system}.serve}/bin/org-roam-ui-lite-serve"
        bundlePath=$(grep -oE '/nix/store/[a-z0-9]+-org-roam-ui-lite-bundle[^/]*' "$origServe" | head -1)

        cp -rL "$bundlePath" "$out"
        chmod -R u+w "$out"

        # Fix baseUrl from "./" to "/" so the browser requests /api/… (absolute),
        # not ./api/… (relative), which resolves incorrectly in some browsers.
        for f in "$out/frontend/dist/assets"/index-*.js; do
          sed 's|baseUrl:"./"|baseUrl:"/"|g' "$f" > "$f.tmp"
          mv "$f.tmp" "$f"
        done
      '';

      fixedServe = pkgs.runCommand "org-roam-ui-lite-serve-fixed" {} ''
        mkdir -p $out/bin
        origServe="${org-roam-ui-lite.packages.${system}.serve}/bin/org-roam-ui-lite-serve"
        origBundle=$(grep -oE '/nix/store/[a-z0-9]+-org-roam-ui-lite-bundle[^/]*' "$origServe" | head -1)

        sed \
          -e 's|serve\.js|serve.mjs|g' \
          -e "s|$origBundle|${patchedBundle}|g" \
          "$origServe" \
          > $out/bin/org-roam-ui-lite-serve
        chmod +x $out/bin/org-roam-ui-lite-serve
      '';

      bobert-view = pkgs.writeShellApplication {
        name = "bobert-view";
        text = ''
          DB="''${BOBERT_ORG_ROAM_DB:-$HOME/.emacs.d/org-roam.db}"
          if [ ! -f "$DB" ]; then
            echo "Error: org-roam database not found at $DB" >&2
            exit 1
          fi
          exec ${fixedServe}/bin/org-roam-ui-lite-serve -d "$DB"
        '';
      };

      bobert = pkgs.writeShellApplication {
        name = "bobert";
        runtimeInputs = [pkgs.rsync];
        text = ''
          CLAUDE_DIR="$HOME/.bobert"
          export CLAUDE_CONFIG_DIR="$HOME/.bobert"
          DATA="${bobertData}"

          for subdir in agents skills hooks output-styles; do
            target="$CLAUDE_DIR/$subdir"
            [ -e "$target" ] && [ ! -d "$target" ] && rm -f "$target"
            mkdir -p "$target"
          done

          rsync --archive --delete "$DATA/agents/" "$CLAUDE_DIR/agents/"
          rsync --archive --delete "$DATA/skills/" "$CLAUDE_DIR/skills/"
          rsync --archive --delete "$DATA/hooks/" "$CLAUDE_DIR/hooks/"
          rsync --archive --delete "$DATA/output-styles/" "$CLAUDE_DIR/output-styles/"

          SETTINGS_DEST="$CLAUDE_DIR/settings.json"
          if ! diff -q "$DATA/settings.json" "$SETTINGS_DEST" > /dev/null 2>&1; then
            rm -f "$SETTINGS_DEST"
            cp "$DATA/settings.json" "$SETTINGS_DEST"
          fi

          find "$CLAUDE_DIR/hooks" "$CLAUDE_DIR/skills" -name "*.sh" -exec chmod +x {} \;

          exec ${pkgs.claude-code}/bin/claude "$@"
        '';
      };
    in {
      packages = {
        inherit bobert bobert-with-emacs bobert-view;
        default = bobert;
      };
      apps = {
        bobert-with-emacs = {
          type = "app";
          program = "${bobert-with-emacs}/bin/bobert-with-emacs";
        };
        bobert-view = {
          type = "app";
          program = "${bobert-view}/bin/bobert-view";
        };
        default = {
          type = "app";
          program = "${bobert}/bin/bobert";
        };
      };
    };
  in {
    packages = forAllSystems (system: (perSystem system).packages);
    apps = forAllSystems (system: (perSystem system).apps);
  };
}
