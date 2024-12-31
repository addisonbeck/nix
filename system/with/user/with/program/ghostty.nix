{ conf, config, pkgs, ...}: 
let
  colors = conf.themes.${conf.activeTheme.colorScheme}.${conf.activeTheme.systemTheme};
  shaderRepo = pkgs.fetchFromGitHub {
    owner = "m-ahdal";
    repo = "ghostty-shaders";
    rev = "main";
    sha256 = "sha256-+fkjR1sTsr4yArDhYQemyhWSOzE9qzDB+lLWaV7TAs4=";
  };
  makeShader = name: {
    target = ".config/ghostty/shaders/${name}";
    source = "${shaderRepo}/${name}";
  };
  shaderFiles = [
    "animated-gradient-shader.glsl"
    "bettercrt.glsl"
    "bloom.glsl"
    "crt.glsl"
    "cubes.glsl"
    "dither.glsl"
    "drunkard.glsl"
    "gears-and-belts.glsl"
    "glitchy.glsl"
    "glow-rgbsplit-twitchy.glsl"
    "gradient-background.glsl"
    "inside-the-matrix.glsl"
    "just-snow.glsl"
    "matrix-hallway.glsl"
    "negative.glsl"
    "retro-terminal.glsl"
    "smoke-and-ghost.glsl"
    "sparks-from-fire.glsl"
    "spotlight.glsl"
    "starfield-colors.glsl"
    "starfield.glsl"
    "tft.glsl"
    "underwater.glsl"
    "water.glsl"
  ];
  shaderConfigs = builtins.listToAttrs (map
    (name: {
      inherit name;
      value = makeShader name;
    })
    shaderFiles
  );
in
{
  home.file = shaderConfigs // {
    ghostty = {
      target = ".config/ghostty/config";
      text = ''
        font-family = "MonaspiceAr Nerd Font Mono"
        font-family-italic = "MonaspiceRn Nerd Font Mono"
        font-family-bold = "MonaspiceKr Nerd Font Mono"
        font-family-bold-italic =  "MonaspiceXe Nerd Font Mono"
        font-thicken = false
        minimum-contrast = 3
        font-size = 13
        macos-titlebar-style = hidden
        palette = 0=${colors.base00}
        palette = 1=${colors.base08}
        palette = 2=${colors.base0B}
        palette = 3=${colors.base09}
        palette = 4=${colors.base0D}
        palette = 5=${colors.base0E}
        palette = 6=${colors.base0C}
        palette = 7=${colors.base04}
        palette = 8=${colors.base03}
        palette = 9=${colors.base08}
        palette = 10=${colors.base0B}
        palette = 11=${colors.base0A}
        palette = 12=${colors.base0D}
        palette = 13=${colors.base0F}
        palette = 14=${colors.base06}
        palette = 15=${colors.base07}
        background = ${colors.base00}
        foreground = ${colors.base05}
        cursor-text = ${colors.base01}
        cursor-color = ${colors.base05}  
        selection-foreground = ${colors.base04}
        selection-background = ${colors.base02}
        window-padding-balance = true
        window-padding-x = 3
        window-padding-y = 3
        clipboard-read = allow
        clipboard-write = allow
        confirm-close-surface = false
        quit-after-last-window-closed = true
        fullscreen = true
        # custom-shader = "${config.home.file."underwater.glsl".source}"
      '';
    };
  };
}
