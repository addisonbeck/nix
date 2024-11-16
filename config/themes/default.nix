{ colors }: let
in {
  spongebob = (import ./spongebob { inherit colors; });
  gruvbox = (import ./gruvbox { inherit colors; });
}
