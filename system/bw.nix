{inputs, ...}: {
  imports = [
    ./with/nix.nix
    ./with/desktop-environment/darwin.nix
    inputs.home-manager.darwinModules.home-manager
    ./with/home-manager.nix
    ./with/trait/allow-unfree-packages.nix
    ./with/user/me.nix
  ];
}
