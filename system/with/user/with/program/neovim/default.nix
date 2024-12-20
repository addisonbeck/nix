# use z<Enter> z. and z- more!
{
  pkgs,
  lib,
  inputs,
  conf,
  ...
}: {
  home.packages = [pkgs.neovim-remote];
  programs.nixvim =
    (import ./nixvim.nix {inherit pkgs lib inputs conf;})
    // {
      enable = true;
    };
}
