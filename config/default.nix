{...}: let
  colors = import ./colors.nix;
in {
  colors = colors;
  themes = import ./themes {inherit colors;};
  activeTheme = {
    colorScheme = import ./color-scheme.nix;
    systemTheme = import ./system-theme.nix;
  };
}
