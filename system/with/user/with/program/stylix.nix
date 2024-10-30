{pkgs, ...}: {
  stylix.enable = true;
  stylix.image = ../wallpaper/empty.png;
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-light-hard.yaml";

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

  # This started overwriting the sign column color and I couldn't figure out
  # why. I style vim with nixvim anyway. It would be nice to unify the two
  # someday. {{{
  stylix.targets.nixvim.enable = false;
  stylix.targets.nixvim.transparentBackground.main = true;
  stylix.targets.nixvim.transparentBackground.signColumn = true;
  # }}}
  stylix.targets.emacs.enable = false;

  # This is broken with Kitty + tmux. Possibly related to https://github.com/danth/stylix/issues/202
  stylix.targets.fish.enable = false;
  stylix.targets.wezterm.enable = false;
}
