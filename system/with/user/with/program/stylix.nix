{ pkgs, ... }: {
  stylix.enable = true;
  stylix.image = ../wallpaper/empty.png;
  stylix.base16Scheme =
    "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
  stylix.fonts = {
    serif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Serif";
    };

    sansSerif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans";
    };

    monospace = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans Mono";
    };

    emoji = {
      package = pkgs.noto-fonts-emoji;
      name = "Noto Color Emoji";
    };
  };
  stylix.fonts.sizes.terminal = 16;
  stylix.opacity.terminal = 0.95;
  stylix.targets.nixvim.transparentBackground.main = true;
}
