{
  config,
  ...
}: {
    age.secrets.freshrss = {
      file = ./freshrss.age;  # Path to your encrypted secret
      owner = config.services.freshrss.user;
      group = config.services.freshrss.group;
      mode = "0400";
    };
}
