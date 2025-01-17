{pkgs, ...}: let
  rustToolchain = pkgs.rust-bin.stable.latest.default.override {
    extensions = ["rust-src"];
  };
in {
  home.packages = with pkgs; [
    rustToolchain
    # rust-analyzer
    # pkg-config
    # cargo
    # clippy
    # rustfmt
  ];
}
