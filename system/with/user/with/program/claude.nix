{pkgs, ...}: let
  bobertOutputStyle = ./claude-output-styles/bobert.md;
in {
  home.packages = [
    pkgs.claude-code-acp
  ];
  home.file.".claude/output-styles".source = bobertOutputStyle;
  programs.claude-code = {
    enable = true;
    settings = {
      outputStyle = "Bobert";
    };
    mcpServers = {
      "org-roam" = {
        type = "stdio";
        command = "/Users/me/binwarden/addisonbeck-org-roam-mcp/main/result/bin/org-roam-mcp";
        env = {
          ORG_ROAM_DIR = "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/roam";
        };
      };
      #filesystem = {
      #  command = "npx";
      #  type = "stdio";
      #  args = [
      #    "-y"
      #    "@modelcontextprotocol/server-filesystem"
      #    "/tmp"
      #    "/Users/me/binwarden"
      #    "/Users/me/notes"
      #    "/Users/me/notes/roam"
      #    "/Users/me/.claude"
      #    "/Users/me/Library/Mobile Documents/com~apple~CloudDocs/notes/"
      #  ];
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
