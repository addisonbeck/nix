{pkgs, ...}: {
  projectRootFile = "flake.nix";
  settings.global.excludes = [
    "*.age"
    "*.md"
    "*.lock"
    "*.yml"
    "*.png"
    "*.tf"
    "*.envrc"
    "*.gitignore"
  ];
  programs = {
    alejandra.enable = true; # nix
    #statix.enable = true; # nix static analysis
    #deadnix.enable = true; # find dead nix code
    shellcheck.enable = true; # bash/shell
    #terraform.enable = true;
    stylua.enable = true;
  };
  settings.formatter = {
    "org" = {
      command = let
        emacs = pkgs.emacs.pkgs.withPackages (epkgs: [epkgs.org]);
      in "${pkgs.writeShellScript "format-org" ''
        ${emacs}/bin/emacs --batch \
          --load ${./format-org.el} \
          "$@"
      ''}";
      includes = ["*.org"];
    };
  };
}
