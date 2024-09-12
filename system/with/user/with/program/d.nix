{ pkgs, ... }: {
  home.packages = [ (pkgs.callPackage ../../../../../tool/d/d.nix { }) ];
}
