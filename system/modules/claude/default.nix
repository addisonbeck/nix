{pkgs, ...}: let
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
      };

      attribution = {
        commit = "";
        pr = "";
      };

      # Hooks configuration
      hooks = {
        UserPromptSubmit = [
          {
            hooks = [
              {
                type = "command";
                command = "~/.claude/skills/agent_reminder/agent_reminder_hook.sh";
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
            matcher = "Write";
            hooks = [
              {
                type = "command";
                command = "~/.claude/hooks/org_roam_protection_hook.sh";
                statusMessage = "Checking org-roam protection...";
              }
            ];
          }
        ];
      };
    };

    # Install skills from local directory
    # Copies skills/ contents to ~/.claude/skills/ at activation
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
}
