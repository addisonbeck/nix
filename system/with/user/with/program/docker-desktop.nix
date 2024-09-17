{
  pkgs,
  lib,
  ...
}: let
  pkgFromApp = {
    name,
    appName ? name,
    version,
    src,
    description,
    homepage,
    buildInputs ? [],
    unpackPhase ? "",
    postInstall ? "",
    sourceRoot ? "${appName}.app",
    ...
  }:
    pkgs.stdenv.mkDerivation {
      name = "${name}-${version}";
      version = "${version}";
      inherit src;
      inherit sourceRoot;
      buildInputs = with pkgs; [undmg unzip] ++ buildInputs;
      phases = ["unpackPhase" "installPhase"];
      inherit unpackPhase;
      installPhase =
        ''
          mkdir -p "$out/Applications/${appName}.app"
          cp -pR * "$out/Applications/${appName}.app"
        ''
        + postInstall;
      meta = with lib; {
        inherit description;
        inherit homepage;
        maintainers = with maintainers; [];
        platforms = platforms.darwin;
      };
    };

  docker-desktop = pkgFromApp rec {
    name = "Docker";
    version = "4.5.0";
    revision = "74594";
    src = builtins.fetchurl {
      url = "https://desktop.docker.com/mac/main/arm64/${revision}/Docker.dmg";
      sha256 = "0161vncg3aq1xlakr0wxsw3lnbxjxc8frqrv6lx9h9cr8rwz7sr4";
    };
    description = "Docker desktop client";
    homepage = "https://docker.com";
  };
in {
  home.packages = [docker-desktop];

  launchd.agents.docker-desktop = {
    enable = true;
    config = {
      ProgramArguments = ["${docker-desktop}/Applications/Docker.app/Contents/MacOS/Docker"];
      KeepAlive = true;
      RunAtLoad = true;
    };
  };
}
