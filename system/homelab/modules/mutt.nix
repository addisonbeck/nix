{ pkgs, config, ... }: {
  age.secrets.do-not-reply-email-password.file = ../../with/user/with/secret/do-not-reply-email-password.age;
  environment.systemPackages = [
    pkgs.mutt
    pkgs.msmtp
  ];
  environment.etc."Muttrc".text = ''
    set ssl_starttls=yes
    set ssl_force_tls=yes
    set imap_user = "do-not-reply@addisonbeck.com"
    set imap_pass = "`cat ${config.age.secrets.do-not-reply-email-password.path}`"
    set from = "do-not-reply@addisonbeck.com"
    set realname = "RSS Bot"

    set folder = "imaps://box.addisonbeck.com:993"
    set spoolfile = "+INBOX"
    set smtp_url = "smtps://do-not-reply@addisonbeck.com@box.addisonbeck.com:465/"
    set smtp_pass = "`cat ${config.age.secrets.do-not-reply-email-password.path}`"
    set ssl_verify_dates = yes
    # vim:ft=muttrc
  '';
}
