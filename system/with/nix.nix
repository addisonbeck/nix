{pkgs, ...}: {
  nix.package = pkgs.nixVersions.latest;
  nix.extraOptions = ''
    experimental-features = nix-command flakes impure-derivations ca-derivations
    keep-outputs = false
    keep-derivations = false
  '';
}
