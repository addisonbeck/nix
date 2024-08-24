{ pkgs, ... }:
let
  binwarden = pkgs.writeShellApplication {
    name = "binwarden";
    runtimeInputs = [binwarden-secrets-provider];
    text = ''
    '';
  }; 

  binwarden-secrets-provider = pkgs.writeShellApplication {
    name = "binwarden-secrets-provider";
    runtimeInputs = [ pkgs.bitwarden-cli ];
    text = ''

    '';
  };

  binwarden-clone-command = pkgs.writeShellApplication {
    name = "binwarden-clone-command";
    text = ''
    '';
  };
in {
  home.packages = [ binwarden ];
}
