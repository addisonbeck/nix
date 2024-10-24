{pkgs, ...}: {
  stylix.enable = true;
  stylix.image = ../wallpaper/empty.png;
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";

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
      package = pkgs.nerdfonts.override {fonts = ["FiraMono"];};
      name = "FiraMono Nerd Font";
    };

    emoji = {
      package = pkgs.noto-fonts-emoji;
      name = "Noto Color Emoji";
    };
  };
  stylix.fonts.sizes.terminal = 20;
  stylix.opacity.terminal = 1.0;
  stylix.targets.nixvim.transparentBackground.main = false;
  stylix.targets.emacs.enable = false;

  # This is broken with Kitty + tmux. Possibly related to https://github.com/danth/stylix/issues/202
  stylix.targets.fish.enable = false;
  stylix.targets.wezterm.enable = false;
}
