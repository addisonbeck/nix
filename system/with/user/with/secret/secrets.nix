let
  me_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH";
  rss_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINCHL5ld7YdBz5uwVhyXmKFR8hcvPkvlcu3leSB/7nHt root@rss.addisonbeck.dev";
in {
  "github.age".publicKeys = [me_key];
  "email.age".publicKeys = [me_key];
  "bw-cal-client-id.age".publicKeys = [me_key];
  "bw-cal-client-secret.age".publicKeys = [me_key];
  "bw-mail-password.age".publicKeys = [me_key];
  "gmail-password.age".publicKeys = [me_key];
  "weechat-plugins-config.age".publicKeys = [me_key];
  "freshrss.age".publicKeys = [me_key rss_key];
}
