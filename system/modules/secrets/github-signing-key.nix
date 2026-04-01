{...}: {
  sops.secrets.github-signing-key = {
    format = "binary";
    sopsFile = ../../../secrets/github-ssh;
    mode = "0400";
  };
}
