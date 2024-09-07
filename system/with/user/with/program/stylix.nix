{ pkgs, ... }: {
  stylix.enable = true;
  stylix.image = ../wallpaper/empty.png;
  stylix.base16Scheme =
    "${pkgs.base16-schemes}/share/themes/gruvbox-light-hard.yaml";

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
      package = pkgs.nerdfonts.override { fonts = [ "FiraMono" ]; };
      name = "FiraMono Nerd Font";
    };

    emoji = {
      package = pkgs.noto-fonts-emoji;
      name = "Noto Color Emoji";
    };
  };
  stylix.fonts.sizes.terminal = 16;
  stylix.opacity.terminal = 0.95;
  stylix.targets.nixvim.transparentBackground.main = true;
  stylix.targets.emacs.enable = false;

  # This is broken with Kitty + tmux. Possibly related to https://github.com/danth/stylix/issues/202
  stylix.targets.fish.enable = false;
}
