{
  inputs,
  outputs,
  nixpkgs,
  pkgs-forked,
  rootPath,
  systemTheme,
  hostname,
  conf,
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
      ;
  };
}
