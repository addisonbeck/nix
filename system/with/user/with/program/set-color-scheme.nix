{pkgs, hostname, ...}: {
  home.packages = [
    (pkgs.writeShellScriptBin "set-color-scheme" ''
      cd ~/nix
      nix develop .#toggle-theme --command nix-set-colorscheme ${hostname} $1
    '')
  ];
}
 
