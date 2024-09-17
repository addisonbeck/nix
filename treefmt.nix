{
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
  };
}
