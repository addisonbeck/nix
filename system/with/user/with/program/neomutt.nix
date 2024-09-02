{ ... }: {
  programs.neomutt.enable = true;
  programs.mbsync.enable = true;
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
      neomutt = { enable = true; };
      mbsync.enable = true;
    };
  };
}
