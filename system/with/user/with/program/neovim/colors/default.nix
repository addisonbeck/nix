{
  systemTheme,
  colorscheme,
  ...
}: {
  options = {
    background = "${systemTheme}";
    termguicolors = false;
  };
  highlightOverrides = {
    SatelliteBackground.link = "SignColumn";
    SatelliteBar.link = "VisualMode";
    MatchParen.link = "Cursor";
    ActiveYank = {
      # "Incandescent Light Bulb
      bg = "#FFBB73";
      fg = "#000000";
    };
    SignColumn = {
      bg = "none";
      ctermbg = "none";
    };
    Nontext = {
      link = "Normal";
    };
  };
  colorscheme = {
    gruvbox = {
      enable = colorscheme == "gruvbox";
      settings = {
        transparent_mode = true;
        overrides = {
          Comment = {
            bold = true;
          };
          Winbar = {
            bold = true;
            fg = 4;
            bg = "NONE";
          };
          WinbarNC = {
            bold = true;
            fg = 8;
            bg = "NONE";
          };
        };
      };
    };
    nord = {
      enable = colorscheme == "nord";
      settings = {
        disable_background = true;
      };
    };
  };
}
