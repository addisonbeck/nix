{
  inputs,
  pkgs,
  ...
}: {
  programs.hyperland.enable = true;
  programs.hyperland.package = inputs.hyperland.packages.${pkgs.stdenv.hostPlatform.system}.hyperland;
}
