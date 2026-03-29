{
  description = "Bobert Claude Code configuration wrapper";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    org-roam-ui-lite = {
      url = "github:tani/org-roam-ui-lite";
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
        inherit bobert;
        default = bobert;
      };
      apps = {
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
