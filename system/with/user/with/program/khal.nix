{
  lib,
  config,
  ...
}: {
  accounts.calendar = {
    basePath = "calendar";
    accounts."addisonbeck_com" = {
      primary = true;
      primaryCollection = "personal";
      khal = {
        enable = true;
        color = "dark blue";
        glob = "*";
        priority = 10;
        readOnly = false;
        type = "discover";
      };
      vdirsyncer = {
        enable = true;
        collections = ["from a" "from b"];
        conflictResolution = "remote wins";
        # TODO: Maybe get this working? Otherwise manually declaring each
        # collection might be smart.
        metadata = ["color"];
      };
      remote = {
        type = "caldav";
        url = "https://box.addisonbeck.com/cloud/remote.php/dav/calendars/me@addisonbeck.com/";
        userName = "me@addisonbeck.com";
      };
      local = {
        type = "filesystem";
        fileExt = ".ics";
      };
    };
    accounts."bitwarden" = {
      khal = {
        enable = true;
        type = "discover";
        glob = "*";
        color = "light red";
      };
      vdirsyncer = {
        enable = true;
        collections = ["from a" "from b"];
        conflictResolution = "remote wins";
        tokenFile = "${config.xdg.dataHome}/calendars/google_token_file";
      };
      remote = {type = "google_calendar";};
      primaryCollection = "addison@bitwarden.com";
    };
  };

  programs.vdirsyncer = {enable = true;};
  services.vdirsyncer = {
    frequency = "*:0/5";
  };

  programs.khal = {
    enable = true;
    locale = {
      dateformat = "%d-%e-%Y";
      datetimeformat = "%A %B %e @ %I:%M%p";
      default_timezone = "EST";
      # Monday is 0, Sunday is 6
      firstweekday = 0;
      #longdateformat = "%A %B %e %Y";
      #longdatetimeformat = "%A %B %e @ %I:%M%p";
      timeformat = "%I:%M%p";
      unicode_symbols = true;
    };
    settings = {
      view = {
        agenda_event_format = lib.strings.concatStrings [
          "{calendar-color}"
          "{calendar}"
          ": "
          "{cancelled}"
          "{start-end-time-style}"
          " "
          "{title}"
          "{repeat-symbol}"
          "{reset}"
        ];
        # TODO This isn't doing what I expect it to
        blank_line_before_day = true;
      };
    };
  };
}
