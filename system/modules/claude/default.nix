{
  pkgs,
  lib,
  ...
}: let
  bobertOutputStyle = ./output-styles/bobert.md;
in {
  # Install Claude Code package
  home.packages = [
    pkgs.claude-code-acp
  ];

  # Install Bobert output style
  home.file.".claude/output-styles".source = bobertOutputStyle;

  # Configure Claude Code
  programs.claude-code = {
    enable = true;

    settings = {
      outputStyle = "Bobert";

      # Environment variables accessible to skills/hooks
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

      # Hooks configuration
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
    };

    # install hooks from local directory
    # copies hooks/ contents to ~/.claude/hooks/ at activation
    hooksDir = ./hooks;

    # install skills from local directory
    # copies skills/ contents to ~/.claude/skills/ at activation
    skillsDir = ./skills;

    # Install agents from local directory
    # Copies agents/ contents to ~/.claude/agents/ at activation
    agentsDir = ./agents;

    # MCP Servers configuration
    mcpServers = {
      #org-roam = {
      #  type = "stdio";
      #  command = "/Users/me/binwarden/addisonbeck-org-roam-mcp/main/result/bin/org-roam-mcp";
      #  env = {
      #    ORG_ROAM_DIR = "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/roam";
      #  };
      #};

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

  # Symlink ai-plugins local clone into ~/.claude/ for live development.
  # Uses activation (not home.file) so edits to the clone are reflected immediately
  # without rebuilding. Handles agents, skills, and commands across all plugins.
  home.activation.aiPluginsSymlinks = lib.hm.dag.entryAfter ["writeBoundary"] ''
    AI_PLUGINS_ROOT="/Users/me/binwarden/bitwarden-ai-plugins/main"
    CLAUDE_DIR="$HOME/.claude"

    if [ -d "$AI_PLUGINS_ROOT/plugins" ]; then
      for plugin_dir in "$AI_PLUGINS_ROOT/plugins"/*/; do
        # Agents: symlink each AGENT.md as a flat <agent-name>.md file
        if [ -d "$plugin_dir/agents" ]; then
          $DRY_RUN_CMD mkdir -p "$CLAUDE_DIR/agents"
          for agent_dir in "$plugin_dir/agents"/*/; do
            [ -d "$agent_dir" ] || continue
            agent_name=$(basename "$agent_dir")
            agent_file="$agent_dir/AGENT.md"
            if [ -f "$agent_file" ]; then
              $DRY_RUN_CMD ln -sfn "$agent_file" "$CLAUDE_DIR/agents/$agent_name.md"
            fi
          done
        fi

        # Skills: symlink each skill subdirectory directly
        if [ -d "$plugin_dir/skills" ]; then
          $DRY_RUN_CMD mkdir -p "$CLAUDE_DIR/skills"
          for skill_dir in "$plugin_dir/skills"/*/; do
            [ -d "$skill_dir" ] || continue
            skill_name=$(basename "$skill_dir")
            $DRY_RUN_CMD ln -sfn "$skill_dir" "$CLAUDE_DIR/skills/$skill_name"
          done
        fi

        # Commands: symlink each <cmd-name>/<cmd-name>.md as a flat <cmd-name>.md file
        if [ -d "$plugin_dir/commands" ]; then
          $DRY_RUN_CMD mkdir -p "$CLAUDE_DIR/commands"
          for cmd_dir in "$plugin_dir/commands"/*/; do
            [ -d "$cmd_dir" ] || continue
            cmd_name=$(basename "$cmd_dir")
            cmd_file="$cmd_dir/$cmd_name.md"
            if [ -f "$cmd_file" ]; then
              $DRY_RUN_CMD ln -sfn "$cmd_file" "$CLAUDE_DIR/commands/$cmd_name.md"
            fi
          done
        fi
      done
    fi
  '';
}
