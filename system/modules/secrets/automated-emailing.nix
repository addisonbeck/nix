{...}: {
  sops.secrets.do-not-reply-email-password = {
    format = "yaml";
    sopsFile = ../../../secrets/automated-emailing.yaml;
    key = "do-not-reply-email-account-password";
    mode = "0600";
  };
}
