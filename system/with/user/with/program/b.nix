{pkgs, ...}: {
  home.packages = [(pkgs.callPackage ../../../../../tool/b/b.nix {})];
}
