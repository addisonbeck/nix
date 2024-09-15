{ inputs, pkgs, ... }: {
  home.packages = [ inputs.binwarden.packages.${pkgs.system}.default ];
}
