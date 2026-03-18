{pkgs, ...}: {
  home.packages = [
    pkgs.python315
    #pkgs.poetry
  ];
}
