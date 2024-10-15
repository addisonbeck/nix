{pkgs, ...}: {
  programs.wezterm = {
    enable = true;
    package = pkgs.wezterm;
    extraConfig = ''
      config.front_end = "WebGpu"
      return config
    '';
  };
}
