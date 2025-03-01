{...}: {
  home.file."leaderkey-config".source = ''
    {
        "type": "group",
        "actions": [
            { "key": "e", "type": "application", "value": "/System/Applications/Utilities/Terminal.app" },
            {
                "key": "o",
                "type": "group",
                "actions": [
                    { "key": "s", "type": "application", "value": "/Applications/Safari.app" },
                    { "key": "e", "type": "application", "value": "/Applications/Mail.app" },
                    { "key": "i", "type": "application", "value": "/System/Applications/Music.app" },
                    { "key": "m", "type": "application", "value": "/Applications/Messages.app" }
                ]
            },
            {
                "key": "r",
                "type": "group",
                "actions": [
                    { "key": "e", "type": "url", "value": "raycast://extensions/raycast/emoji-symbols/search-emoji-symbols" },
                    { "key": "p", "type": "url", "value": "raycast://confetti" },
                    { "key": "c", "type": "url", "value": "raycast://extensions/raycast/system/open-camera" }
                ]
            }
        ]
    }
  '';
}
