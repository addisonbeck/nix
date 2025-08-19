let
  me_key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIG1Q5KRw6Tx5e2mjzr/MQL+XGZA4XEF43b+xbnzL6vcYAAAABHNzaDo= me@addisonbeck.com-20250819";
  rss_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINCHL5ld7YdBz5uwVhyXmKFR8hcvPkvlcu3leSB/7nHt root@rss.addisonbeck.dev";
  homelab_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKw0bNG37Fcq1QHglvMY+nAxhRLtqJ8pv8ph9F2SFckm root@raspberrypi";
  bw_host_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF25XOpY1SCpIa+pGLZThCklekC4hXdV+mW8k56+6Lhg root@bw";
  old_me_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJJSLY/c9uffjNA0T8o8CjrAI7DdvxNyp0SNBeLjQ4pH";
  # I actually don't need this here. It just needs to be a known host for
  # SSH-able machines (droplets, the homelab, the mail server, etc.) Agenix
  # doesn't support this encryption type, so I still need a more traditional
  # key (me) to be my master "decrypt the secrets" key.
  yk_key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBNscDEbErRCzRobR/U+vIW7eCWbOzKyv9PLigyXyGGTAi/9cacxxFZMXvjtQi7opZfe0+kgUXf+4SbGB8pTeqEg=";
in {
  "github.age".publicKeys = [me_key yk_key];
  "freshrss.age".publicKeys = [me_key rss_key homelab_key old_me_key];
  "authinfo.age".publicKeys = [me_key yk_key];
  "homelab-grafana-admin-password.age".publicKeys = [me_key homelab_key old_me_key];
  "homelab-adguard-admin-password.age".publicKeys = [me_key homelab_key old_me_key];
  "homelab-vaultwarden-env-file.age".publicKeys = [me_key homelab_key old_me_key];
  "do-not-reply-email-password.age".publicKeys = [me_key homelab_key bw_host_key old_me_key];
}
