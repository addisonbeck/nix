{ ... }: let
  colors = {
    spongebob = {
      rockBottomNight0 = "#110f15";
      bikiniBottomNight0 = "#050a1b";
      bikiniBottomNight1 = "#050a1b";
      bikiniBottomNight2 = "#2d9db8"; 
      gooLagoon = "#254657";
      bikiniBottomSand = "D8E4B9";
      krustyKrabFloor = "#3A662F";
      chumBucketGlove = "#637bc8";
      mrKrabsRed = "#FF3B3f";
      mermaidManOrange = "#FBAC12";
      spongebobYellow = "#F9E500";
      planktonGreen = "#3A662F";
      garyBlue = "#77BCDD";
      bikiniBottomDay0 = "#00A9E0";
      jellyfishPurple = "#D06D98";
      spongebobsPantsBrown = "#9E7A38";
    };
    gruvbox = {
      hard = {
        dark = {
          bg1 = "#1d2021"; # ----
          bg2 = "#3c3836"; # ---
          bg3 = "#504945"; # --
          bg4 = "#665c54"; # -
          fg1 = "#bdae93"; # +
          fg2 = "#d5c4a1"; # ++
          fg3 = "#ebdbb2"; # +++
          fg4 = "#fbf1c7"; # ++++
          red = "#fb4934"; 
          orange = "#fe8019"; 
          yellow = "#fabd2f"; 
          green = "#b8bb26"; 
          aqua = "#8ec07c"; 
          blue = "#83a598";
          purple = "#d3869b";
          brown = "#d65d0e";
        };
        light = {
          bg1 = "#f9f5d7";
          bg2 = "#ebdbb2";
          bg3 = "#d5c4a1";
          bg4 = "#bdae93";
          fg1 = "#665c54";
          fg2 = "#504945";
          fg3 = "#3c3836";
          fg4 = "#282828";
          red = "#9d0006";
          orange = "#af3a03";
          yellow = "#b57614";
          green = "#79740e";
          aqua = "#427b58";
          blue = "#076678";
          purple = "#8f3f71";
          brown = "#d65d0e";
        };
      };
    };
  };
  /**
  The base16 color scheme schema looks like this:
  ```
    base00 = ""; # Default Background
    base01 = ""; # Lighter Background
    base02 = ""; # Selection Background
    base03 = ""; # Comments, Invisibles, Line Highlighting
    base04 = ""; # Dark Foreground
    base05 = ""; # Default Foreground
    base06 = ""; # Light Foreground
    base07 = ""; # Light Background
    base08 = ""; # Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
    base09 = ""; # Integers, Boolean, Constants, XML Attributes, Markup Link URL
    base0A = ""; # Classes, Markup Bold, Search Text Background
    base0B = ""; # Strings, Inherited Class, Markup Code, Diff Inserted
    base0C = ""; # Support, Regular Expressions, Escape Characters, Markup Quotes
    base0D = ""; # Function, Methods, Attribute IDs, Headings
    base0E = ""; # Keywords, Storage, Selector, Markup Italic, Diff Changed
    base0F = ""; # Deprecated, Opening/Closing Embedded Language Tags, eg <?php?>
  ```
  */
  themes = {
    spongebob = {
      dark = {
        base00 = colors.spongebob.rockBottomNight0; 
        base01 = colors.spongebob.bikiniBottomNight0; 
        base02 = colors.spongebob.bikiniBottomNight1; 
        base03 = colors.spongebob.bikiniBottomNight2; 
        base04 = colors.spongebob.gooLagoon; 
        base05 = colors.spongebob.bikiniBottomSand; 
        base06 = colors.spongebob.kurstyKrabFloor; 
        base07 = colors.spongebob.chumBucketGlove; 
        base08 = colors.spongebob.mrKrabsRed; 
        base09 = colors.spongebob.mermaidManOrange; 
        base0A = colors.spongebob.spongebobYellow; 
        base0B = colors.spongebob.planktonGreen; 
        base0C = colors.spongebob.garyBlue; 
        base0D = colors.spongebob.bikiniBottomDay0; 
        base0E = colors.spongebob.jellyfishPurple; 
        base0F = colors.spongebob.spongebobsPantsBrown; 
      };
    };
    gruvbox = {
      dark = {
        base00 = colors.gruvbox.hard.dark.bg1;
        base01 = colors.gruvbox.hard.dark.bg2;
        base02 = colors.gruvbox.hard.dark.bg3;
        base03 = colors.gruvbox.hard.dark.bg4;
        base04 = colors.gruvbox.hard.dark.fg1;
        base05 = colors.gruvbox.hard.dark.fg2;
        base06 = colors.gruvbox.hard.dark.fg3;
        base07 = colors.gruvbox.hard.dark.fg4;
        base08 = colors.gruvbox.hard.dark.red;
        base09 = colors.gruvbox.hard.dark.orange;
        base0A = colors.gruvbox.hard.dark.yellow;
        base0B = colors.gruvbox.hard.dark.green;
        base0C = colors.gruvbox.hard.dark.aqua;
        base0D = colors.gruvbox.hard.dark.blue;
        base0E = colors.gruvbox.hard.dark.purple;
        base0F = colors.gruvbox.hard.dark.brown;
      };
      light = {
        base00 = colors.gruvbox.hard-light.bg1;
        base01 = colors.gruvbox.hard.light.bg2;
        base02 = colors.gruvbox.hard.light.bg3;
        base03 = colors.gruvbox.hard.light.bg4;
        base04 = colors.gruvbox.hard.light.fg1;
        base05 = colors.gruvbox.hard.light.fg2;
        base06 = colors.gruvbox.hard.light.fg3;
        base07 = colors.gruvbox.hard.light.fg4;
        base08 = colors.gruvbox.hard.light.red;
        base09 = colors.gruvbox.hard.light.orange;
        base0A = colors.gruvbox.hard.light.yellow;
        base0B = colors.gruvbox.hard.light.green;
        base0C = colors.gruvbox.hard.light.aqua;
        base0D = colors.gruvbox.hard.light.blue;
        base0E = colors.gruvbox.hard.light.purple;
        base0F = colors.gruvbox.hard.light.brown;
      };
    };
  };
in {
  colors = colors;
  themes = themes;
}
