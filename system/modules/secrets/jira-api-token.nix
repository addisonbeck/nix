{config, lib, ...}: {
  sops.secrets = {
    "JIRA_API_TOKEN" = {
      format = "binary";
      sopsFile = ../../../secrets/jira-token;
      mode = "0400";
    };
  };

  programs.fish = lib.mkIf config.programs.fish.enable {
    shellInit = ''
      export JIRA_API_TOKEN="$(cat ${config.sops.secrets."JIRA_API_TOKEN".path})"
    '';
  };

  programs.bash = lib.mkIf config.programs.bash.enable {
    initExtra = ''
      export JIRA_API_TOKEN="$(cat ${config.sops.secrets."JIRA_API_TOKEN".path})"
    '';
  };
}
