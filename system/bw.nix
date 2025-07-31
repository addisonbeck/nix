{inputs, ...}: {
  imports = [
    ./with/nix.nix
    ./with/nix-darwin.nix
    ./with/desktop-environment/darwin.nix
    inputs.home-manager.darwinModules.home-manager
    ./with/home-manager.nix
    ./with/trait/allow-unfree-packages.nix
    ./with/user/me.nix
    ./modules/automated-emailing
  ];

  # This is required for the automated-emailing module to work on mac, but
  # any time I try and do something fancy to conditionally add it for darwin
  # in the module itself I end up with a bunch of errors.
  homebrew = {
    enable = true;
    casks = [ "calibre" ];
  };
}
