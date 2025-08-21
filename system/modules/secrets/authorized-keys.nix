{...}: {
  # This doesn't ever really work and I just don't encrypt authorized_keys.
  # On future iterations I'd like to figure out how to make this work.
  environment.etc."ssh/authorized_keys" = {
    source = ../../../secrets/authorized_keys;
    mode = "0444";
  };
}
