{...}: {
  home.file.".config/leaderkey/config.json".text = ''
    {
      "actions" : [
        {
          "key" : "e",
          "label" : "emacs",
          "type" : "command",
          "value" : "ec"
        },
        {
          "key" : "s",
          "label" : "",
          "type" : "application",
          "value" : "/System/Volumes/Preboot/Cryptexes/App/System/Applications/Safari.app"
        },
        {
          "key" : "k",
          "label" : "",
          "type" : "application",
          "value" : "/Applications/Slack.app"
        },
        {
          "key" : "m",
          "label" : "",
          "type" : "application",
          "value" : "/System/Applications/Mail.app"
        },
        {
          "key" : "t",
          "label" : "",
          "type" : "application",
          "value" : "/System/Applications/Messages.app"
        },
        {
          "key" : "b",
          "label" : "",
          "type" : "application",
          "value" : "/Applications/Bitwarden.app"
        },
        {
          "key" : "c",
          "label" : "",
          "type" : "application",
          "value" : "/System/Applications/Calendar.app"
        }
      ],
      "type" : "group"
    }
  '';
}
