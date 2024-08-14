{ pkgs, lib, inputs, ... }: {
  programs.hyperland.enable = true;
  programs.hyperland.package =
    inputs.hyperland.packages."${pkgs.system}".hyperland;
}
