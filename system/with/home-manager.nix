{
  inputs,
  outputs,
  nixpkgs,
  pkgs-forked,
  rootPath,
  systemTheme,
  hostname,
  conf,
  emacs-overlay,
  ...
}: {
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.extraSpecialArgs = {
    inherit
      inputs
      outputs
      nixpkgs
      pkgs-forked
      rootPath
      systemTheme
      hostname
      conf
      emacs-overlay
      ;
  };
}
