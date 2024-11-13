{...}: {
  programs.kitty.enable = true;
  programs.kitty.settings.hide_window_decorations = "no";
  programs.kitty.settings.confirm_os_window_close = "0";
  programs.kitty.settings.window_padding_width = "0 0";
  programs.kitty.settings.undercurl_style = "thin-sparse";
  programs.kitty.settings.cursor_blink_interval = "0.5";
  programs.kitty.settings.cursor_stop_blinking_after = "0";
  programs.kitty.shellIntegration.mode = "no-cursor";
  programs.kitty.settings.cursor_shape = "block";
  programs.kitty.settings.cursor_unfocused = "hollow";
  programs.kitty.extraConfig = ''
    modify_font strikethrough_position 120%
    modify_font strikethrough_thickness 250%
    modify_font underline_position 150%
    modify_font underline_thickness 1px
    modify_font cell_height 100%
    bold_font        MonaspiceAr Nerd Font Mono Bold
    italic_font      MonaspiceRn Nerd Font Mono Italic
    bold_italic_font MonaspiceRn Nerd Font Mono Bold Italic
  '';
}
