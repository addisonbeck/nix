{ inputs, pkgs, lib, ... }: {
  programs.zsh.enable = true;
  programs.fish.enable = true;

  environment.shells = [ pkgs.zsh pkgs.fish ];

  users.knownUsers = [ "me" ];

  #users.users.me = {
    #shell = pkgs.fish;
  #} // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    #uid = 503;
    #name = "me";
    #home = "/Users/me";
    #createHome = true;
  #} // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
    #users.users.me.isNormalUser = true;
    #users.users.me.initialPassword = "me";
    #users.users.me.extraGroups = [ "wheel" "docker" ];
  #};
}
