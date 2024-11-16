{
  systemTheme,
  colorscheme,
  colors,
  ...
}: {
  options = {
    background = "${systemTheme}";
    termguicolors = false;
  };
  colorscheme = {
    gruvbox = {
      enable = colorscheme == "gruvbox";
      settings = {
        transparent_mode = true;
        overrides = {
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
          "@function" = {
            italic = false;
          };
          "@function.builtin" = {
            italic = false;
          };
          "@markup.list.checked.markdown" = {
            italic = true;
            fg = "#b8bb26";
          };
          Comment = {
            bold = false;
            italic = true;
          };
          Winbar = {
            bold = true;
            #fg = 4;
            bg = "NONE";
          };
          WinbarNC = {
            bold = true;
            #fg = 8;
            bg = "NONE";
          };
          # "@markup.raw.markdown_inline" = {
          #   bg = colors.themes.${colorscheme}.${systemTheme}.base01;
          #   fg = colors.themes.${colorscheme}.${systemTheme}.base09;
          #   italic = false;
          #   bold = true;
          # };
          # "@markup.link.label.markdown_inline" = {
          #   link = colors.themes.${colorscheme}.${systemTheme}.base0D;
          # };
          # "@markup.quote.markdown" = {
          #   italic = true;
          #   bold = false;
          #   bg = colors.themes.${colorscheme}.${systemTheme}.base02;
          #   fg = colors.themes.${colorscheme}.${systemTheme}.base0E;
          # };
          # "@markup.heading" = {
          #   underdotted = true;
          #   bold = true;
          #   italic = true;
          # };
        };
      };
    };
  };
}
