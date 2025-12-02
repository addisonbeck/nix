{
  inputs,
  pkgs,
  lib,
  ...
}: {
  # Allow unfree packages for the testing VM
  nixpkgs.config.allowUnfree = true;
  
  # Enable fish shell
  programs.fish.enable = true;
  environment.shells = [pkgs.fish];
  
  # Define the testing user account
  users.users.testing = {
    isNormalUser = true;
    initialPassword = "testing123";  # Insecure password for VM testing only
    extraGroups = [
      "wheel"          # sudo access
      "networkmanager" # network configuration
      "docker"         # docker access
      "audio"          # audio access
    ];
    shell = pkgs.fish;
  };
  
  # Home-manager configuration for testing user
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.testing = {
    home.stateVersion = "24.11";
    
    # Development packages for Bitwarden client work
    home.packages = with pkgs; [
      # Build tools
      ripgrep
      fd
      bat
      eza
      
      # Network tools for debugging
      wireshark
      tcpdump
      netcat-gnu
      
      # Performance monitoring
      iotop
      nethogs
      
      # Text processing
      jq
      yq
      
      # Archive handling
      unzip
      p7zip
    ];
    
    # Git configuration for development
    programs.git = {
      enable = true;
      userName = "Testing User";
      userEmail = "testing@bitwarden-test.local";
      extraConfig = {
        init.defaultBranch = "main";
        pull.rebase = false;
      };
    };
    
    # Fish shell configuration
    programs.fish = {
      enable = true;
      shellInit = ''
        # Testing VM greeting
        set -g fish_greeting "Bitwarden Testing VM Ready!"
        
        # Useful aliases for testing
        alias ll='eza -la'
        alias grep='rg'
        alias cat='bat'
        
        # Development shortcuts
        alias build-client='npm run build'
        alias test-client='npm run test'
        alias dev-client='npm run dev'
      '';
    };
    
    # Terminal configuration
    programs.kitty = {
      enable = true;
      settings = {
        font_size = 12;
        background_opacity = "0.95";
        confirm_os_window_close = 0;
      };
    };
    
    # Environment variables for development
    home.sessionVariables = {
      EDITOR = "code";
      BROWSER = "firefox";
      # Node.js development
      NODE_ENV = "development";
      # Increase Node.js memory limit for large builds
      NODE_OPTIONS = "--max-old-space-size=4096";
    };
    
    # XDG configuration for proper desktop integration
    xdg.enable = true;
    xdg.mimeApps.enable = true;
    
    # GTK theme configuration for consistency
    gtk = {
      enable = true;
      theme = {
        name = "Breeze-Dark";
        package = pkgs.kdePackages.breeze-gtk;
      };
    };
  };
}
