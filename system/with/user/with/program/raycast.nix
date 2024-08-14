{ pkgs, ... }: {
  home.packages = [ pkgs.raycast ];

  launchd.agents.raycast = {
    enable = true;
    config = {
      ProgramArguments =
        [ "${pkgs.raycast}/Applications/Raycast.app/Contents/MacOS/Raycast" ];
      KeepAlive = true;
      RunAtLoad = true;
    };
  };
}
