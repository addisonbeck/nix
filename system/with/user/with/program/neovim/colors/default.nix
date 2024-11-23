{
  conf,
  ...
}: {
  options = {
    background = "${conf.activeTheme.systemTheme}";
    termguicolors = false;
  };
  highlight = {
    CodeFence = {
      bg = conf.themes.${conf.activeTheme.colorScheme}.${conf.activeTheme.systemTheme}.base01;
    };
  };
  colorscheme = {
    gruvbox = {
      enable = conf.activeTheme.colorScheme == "gruvbox";
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
          "@markup.raw.markdown_inline" = {
            bg = conf.themes.${conf.activeTheme.colorScheme}.${conf.activeTheme.systemTheme}.base01;
            fg = conf.themes.${conf.activeTheme.colorScheme}.${conf.activeTheme.systemTheme}.base09;
            italic = false;
            bold = true;
          };
          # "@markup.raw.block.markdown" = {
          #   bg = conf.themes.${conf.activeTheme.colorScheme}.${conf.activeTheme.systemTheme}.base01;
          # };
          "@markup.link.label.markdown_inline" = {
            link = "GruvboxBlue";
          };
          "@markup.quote.markdown" = {
            italic = true;
            bold = false;
            bg =
            conf.themes.${conf.activeTheme.colorScheme}.${conf.activeTheme.systemTheme}.base01;
            fg =
            conf.themes.${conf.activeTheme.colorScheme}.${conf.activeTheme.systemTheme}.base0E;
          };
          "@markup.heading" = {
            underdotted = true;
            bold = true;
            italic = true;
          };
        };
      };
    };
  };
}
