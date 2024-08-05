{
  pkgs,
  ...
}: {
  programs.kitty.enable = true;
  programs.kitty.font.size = 16;
  programs.kitty.font.name = "FiraCode Nerd Font";
  programs.kitty.font.package = pkgs.fira-code-nerdfont;
  programs.kitty.settings.background_opacity = "0.8";
  programs.kitty.settings.hide_window_decorations = "no";
  programs.kitty.settings.confirm_os_window_close = "0";
  programs.kitty.settings.window_padding_width = "10 30";
}
