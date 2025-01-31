{
  config,
  ...
}: {
    age.secrets.freshrss = {
      file = ./freshrss.age;  # Path to your encrypted secret
      owner = config.services.freshrss.user;
      mode = "0400";
    };
}
