{
  inputs,
  outputs,
  nixpkgs,
  pkgs-forked,
  rootPath,
  systemTheme,
  hostname,
  colorscheme,
  colors,
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
      colorscheme
      colors
      ;
  };
}
