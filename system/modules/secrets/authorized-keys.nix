{...}: {
  environment.etc."ssh/authorized_keys" = {
    source = ../../../secrets/authorized_keys;
    mode = "0444";
  };
}
