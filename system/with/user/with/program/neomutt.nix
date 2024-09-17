{...}: {
  programs.neomutt.enable = true;
  programs.neomutt.sidebar.enable = true;
  programs.mbsync.enable = true;
  services.mbsync = {
    frequency = "*:0/5";
  };
  accounts.email = {
    maildirBasePath = "mail";
    accounts."addisonbeck_com" = {
      primary = true;
      address = "me@addisonbeck.com";
      userName = "me@addisonbeck.com";
      realName = "Addison Beck";
      imap.host = "box.addisonbeck.com";
      smtp.host = "box.addisonbeck.com";
      #folders.inbox = "virtual.all";
      neomutt = {enable = true;};
      mbsync = {
        enable = true;
        create = "both";
        remove = "both";
        expunge = "both";
      };
    };
    accounts."bitwarden" = {
      flavor = "gmail.com";
      address = "addison@bitwarden.com";
      userName = "addison@bitwarden.com";
      realName = "Addison Beck";
      neomutt = {enable = true;};

      mbsync = {
        enable = true;
        create = "both";
        remove = "both";
        expunge = "both";
      };
    };
    accounts."gmail" = {
      flavor = "gmail.com";
      address = "addisonbeck1@gmail.com";
      userName = "addisonbeck1@gmail.com";
      realName = "Addison Beck";
      neomutt = {enable = true;};

      mbsync = {
        enable = true;
        create = "both";
        remove = "both";
        expunge = "both";
      };
    };
  };
}
