{
  inputs,
  pkgs,
  ...
}: {
  programs.hyprland.enable = true;
  #programs.hyperland.package = inputs.hyperland.packages.${pkgs.stdenv.hostPlatform.system}.hyperland;
}
