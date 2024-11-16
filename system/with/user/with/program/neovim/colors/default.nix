{
  systemTheme,
  colorscheme,
  ...
}: let
  markdownInlineCode = {
    dark = {
      bg = "#282828";
      fg = "#d3869b"; # orange
    };
    light = {
      bg = "#d5c4a1";
      fg = "#af3a03"; # orange
    };
  };
  markdownQuotes = {
    dark = {
      bg = "#3c3836";
      fg = "#fe8019"; # purple
    };
    light = {
      bg = "#ebdbb2";
      fg = "#8f3f71"; # purple
    };
  };
in {
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
  };
  colorscheme = {
    gruvbox = {
      enable = colorscheme == "gruvbox";
      settings = {
        transparent_mode = true;
        overrides = {
          Comment = {
            bold = false;
            italic = true;
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
          "@markup.raw.markdown_inline" = {
            bg = markdownInlineCode.${systemTheme}.bg;
            fg = markdownInlineCode.${systemTheme}.fg;
            italic = false;
            bold = true;
          };
          "@markup.link.label.markdown_inline" = {
            link = "GruvboxBlue";
          };
          "@markup.quote.markdown" = {
            italic = true;
            bold = false;
            bg = markdownQuotes.${systemTheme}.bg;
            fg = markdownQuotes.${systemTheme}.fg;
          };
          "@markup.heading" = {
            underdotted = true;
            bold = true;
            italic = true;
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
