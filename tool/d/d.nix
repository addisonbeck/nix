{ stdenv }:

let version = "0.0.1";
in stdenv.mkDerivation {

  name = "d-${version}";

  src = ./.;

  installPhase = ''
    mkdir -p $out
    cp -R ./bin $out/bin
    cp -R ./lib $out/lib
  '';

}
