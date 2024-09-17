{...}: {
  # Homebrew can't be managed through nix, but adding these paths here is
  # helpful for configs.
  home.sessionPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
}
