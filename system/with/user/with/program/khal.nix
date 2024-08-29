{ ... }: {
  accounts.calendar = {
    basePath = "calendar";
    accounts."addisonbeck_com" = {
      primary = true;
      primaryCollection = "personal";
      khal = {
        enable = true;
        color = "light green";
        glob = "*";
        priority = 10;
        readOnly = false;
        type = "discover";
      };
      vdirsyncer = {
        enable = true;
        collections = [ "from a" "from b" ];
        conflictResolution = "remote wins";
      };
      remote = {
        type = "caldav";
        url =
          "https://box.addisonbeck.com/cloud/remote.php/dav/calendars/me@addisonbeck.com/";
        userName = "me@addisonbeck.com";
      };
      local = {
        type = "filesystem";
        fileExt = ".ics";
      };
    };
  };

  programs.vdirsyncer = { enable = true; };

  programs.khal = {
    enable = true;
    locale = {
      # Format strings are for Python strftime, similarly to strftime(3).
      dateformat = "%x";
      datetimeformat = "%c";
      default_timezone = "EST";
      # Monday is 0, Sunday is 6
      firstweekday = 0;
      longdateformat = "%x";
      longdatetimeformat = "%c";
      timeformat = "%X";
      #unicode_symbols = true;
    };
    # settings = {
    #   view = {
    #     agenda_event_format =
    #       "{calendar-color}{cancelled}{start-end-time-style} {title}{repeat-symbol}{reset}";
    #   };
    # };
  };
}
