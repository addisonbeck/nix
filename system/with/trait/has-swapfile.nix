{ lib, config, ... }:
with lib;
let cfg = config.has-swapfile;
in {
  options.has-swapfile = {
    sizeGb = mkOption {
      type = types.int;
      default = 1;
    };
  };

  config = {
    swapDevices = [{
      device = "/swapfile";
      size = 1024 * cfg.sizeGb;
    }];
  };
}
