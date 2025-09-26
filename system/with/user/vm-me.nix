{
  inputs,
  pkgs,
  lib,
  ...
}: {
  # Allow unfree packages for the VM
  nixpkgs.config.allowUnfree = true;
  
  # Enable fish shell
  programs.fish.enable = true;
  environment.shells = [pkgs.fish];
  
  # Define the user account
  users.users.me = {
    isNormalUser = true;
    initialPassword = "me";
    extraGroups = ["wheel" "networkmanager"];
    shell = pkgs.fish;
  };
  
  # Basic home-manager configuration (minimal for VM)
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.me = {
    home.stateVersion = "24.05";
    
    # Basic packages
    home.packages = with pkgs; [
      ripgrep
      wget
      git
    ];
    
    # Basic fish configuration
    programs.fish.enable = true;
    programs.fish.shellInit = ''
      # Basic shell configuration
      set -g fish_greeting "Hi Addison!"
    '';
    
    # Basic git configuration
    programs.git = {
      enable = true;
      userName = "me";
      userEmail = "me@vm.local";
    };
    
    # Environment variables
    home.sessionVariables = {
      EDITOR = "emacs";
    };
  };
}
