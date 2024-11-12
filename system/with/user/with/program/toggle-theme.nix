#defaults write -g NSRequiresAquaSystemAppearance -bool ${newSystemTheme.darwinBool}
{pkgs, hostname, ...}: {
  home.packages = [
    (pkgs.writeShellScriptBin "toggle-theme" ''
      cd ~/nix
      nix develop .#toggle-theme --command nix-toggle-theme ${hostname}
    '')
  ];
}
 
