{ inputs, pkgs, ... }: {
  home.packages = [ inputs.d.packages.${pkgs.system}.default ];
}
