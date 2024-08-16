{ ... }: {
  programs.fish.enable = true;
  programs.fish.shellInit = ''
    # Disable greeting text.
    set -g fish_greeting
  '';
}
