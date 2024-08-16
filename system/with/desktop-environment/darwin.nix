{ inputs, pkgs, ... }: {
  services.nix-daemon.enable = true;

  # Avoids a logout/login cycle
  system.activationScripts.postUserActivation.text = ''
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  '';

  # Turns on key mapping
  system.keyboard.enableKeyMapping = true;

  # Remaps caps lock to escape
  system.keyboard.remapCapsLockToEscape = true;

  # Whether to install documentation of packages from environment.systemPackages into the generated system path.
  documentation.enable = true;

  # Whether to install documentation distributed in packages’ /share/doc. Usually plain text and/or HTML. This also includes “doc” outputs.
  documentation.doc.enable = true;

  # Whether to install info pages and the info command. This also includes “info” outputs.
  documentation.info.enable = true;

  # Whether to install manual pages and the man command. This also includes “man” outputs.
  documentation.man.enable = true;

  # Shell script code called during global environment initialisation after all variables and 
  # profileVariables have been set. This code is asumed to be shell-independent, which means you 
  # should stick to pure sh without sh word split.
  environment.extraInit = "";

  # List of additional package outputs to be symlinked into /run/current-system/sw.
  environment.extraOutputsToInstall = [ "doc" "info" "devdoc" "man" ];

  # Shell script code called during interactive shell initialisation. 
  # This code is asumed to be shell-independent, which means you should stick to pure sh without sh word split.
  environment.interactiveShellInit = "";

  # TODO: Is this like systemd? Does home-manager do this?
  # Set of files that have to be linked in /Library/LaunchAgents.
  environment.launchAgents = { };

  # Set of files that have to be linked in /Library/LaunchDaemons.
  environment.launchDaemons = { };

  # Configure default login shell.
  environment.loginShell = pkgs.zsh;

  # I do not totally understand what this does
  environment.etc.darwin.source = "${inputs.nix-darwin}";

  # Shell script code called during login shell initialisation. 
  # This code is asumed to be shell-independent, which means you should stick to pure sh without sh word split.
  environment.loginShellInit = "";

  #List of directories to be symlinked in /run/current-system/sw.
  environment.pathsToLink = [ "/Applications" ];

  # An attribute set that maps aliases (the top level attribute names in this option) to command strings or directly to build outputs. The alises are added to all users’ shells.
  environment.shellAliases = { };

  # Shell script code called during shell initialisation. This code is asumed to be shell-independent, which means you should stick to pure sh without sh word split.
  environment.shellInit = ''
    defaults write -g NSRequiresAquaSystemAppearance -bool true
  '';

  # A list of permissible login shells for user accounts. No need to mention /bin/sh and other shells that are available by default on macOS.
  environment.shells = [ pkgs.zsh ];

  # TODO there is a typo in the nix-darwin documentation here.
  # The set of packages that appear in /run/current-system/sw. 
  # These packages are automatically available to all users, and are automatically updated every time you rebuild the system configuration. 
  # (The latter is the main difference with installing them in the default profile, /nix/var/nix/profiles/default.
  environment.systemPackages = [ ];

  # The nix-darwin docs are missing a default value here
  # The set of paths that are added to PATH.
  environment.systemPath = [ "/opt/homebrew/bin" "/opt/homebrew/sbin" ];

  # Set of files that have to be linked in ~/Library/LaunchAgents.
  environment.userLaunchAgents = { };

  # A set of environment variables used in the global environment. 
  # These variables will be set on shell initialisation. 
  # The value of each variable can be either a string or a list of strings. 
  # The latter is concatenated, interspersed with colon characters.
  environment.variables = { };

  # List of fonts to install into /Library/Fonts/Nix Fonts.
  fonts.packages = [ ];

  # NOTE: There are a lot of other homebrew options, like configuring services, download locations, and auto-update settings
  # Whether to enable nix-darwin to manage installing/updating/upgrading Homebrew taps, formulae, and casks, as well as Mac App Store apps and Docker containers, using Homebrew Bundle.
  # Note that enabling this option does not install Homebrew, see the Homebrew website for installation instructions.
  # Use the homebrew.brews, homebrew.casks, homebrew.masApps, and homebrew.whalebrews options to list the Homebrew formulae, casks, Mac App Store apps, and Docker containers you’d like to install. Use the homebrew.taps option, to make additional formula repositories available to Homebrew. This module uses those options (along with the homebrew.caskArgs options) to generate a Brewfile that nix-darwin passes to the brew bundle command during system activation.
  # The default configuration of this module prevents Homebrew Bundle from auto-updating Homebrew and all formulae, as well as upgrading anything that’s already installed, so that repeated invocations of darwin-rebuild switch (without any change to the configuration) are idempotent. You can modify this behavior using the options under homebrew.onActivation.
  # This module also provides a few options for modifying how Homebrew commands behave when you manually invoke them, under homebrew.global.
  homebrew.enable = true;

  # List of Homebrew formulae to install.
  homebrew.brews = [ ];

  # List of Homebrew casks to install.
  homebrew.casks = [ ];

  # Applications to install from Mac App Store using mas.
  # When this option is used, "mas" is automatically added to homebrew.brews.
  # Note that you need to be signed into the Mac App Store for mas to successfully install and upgrade applications, and that unfortunately apps removed from this option will not be uninstalled automatically even if homebrew.onActivation.cleanup is set to "uninstall" or "zap" (this is currently a limitation of Homebrew Bundle).
  # For more information on mas see: github.com/mas-cli/mas.
  # homebrew.masApps = [];

  # List of Docker images to install using whalebrew.
  homebrew.whalebrews = [ ];

  # TODO: Again, this seems like systemd and is probably managed better in home-manager. There are a lot of nix-darwin options for bth launchd.agents and launchd.daemons
  # Definition of per-user launchd agents.
  # When a user logs in, a per-user launchd is started. It does the following:
  # It loads the parameters for each launch-on-demand user agent from the property list files found in /System/Library/LaunchAgents, /Library/LaunchAgents, and the user’s individual Library/LaunchAgents directory.
  # It registers the sockets and file descriptors requested by those user agents.
  # It launches any user agents that requested to be running all the time.
  # As requests for a particular service arrive, it launches the corresponding user agent and passes the request to it.
  # When the user logs out, it sends a SIGTERM signal to all of the user agents that it started.
  launchd.agents = { };

  # Definition of launchd daemons.
  # After the system is booted and the kernel is running, launchd is run to finish the system initialization. As part of that initialization, it goes through the following steps:
  # It loads the parameters for each launch-on-demand system-level daemon from the property list files found in /System/Library/LaunchDaemons/ and /Library/LaunchDaemons/.
  # It registers the sockets and file descriptors requested by those daemons.
  # It launches any daemons that requested to be running all the time.
  # As requests for a particular service arrive, it launches the corresponding daemon and passes the request to it.
  # When the system shuts down, it sends a SIGTERM signal to all of the daemons that it started.
  launchd.daemons = { };

  # This option allows modules to define helper functions, constants, etc.
  lib = { };

  # The user-friendly name for the system, set in System Preferences > Sharing > Computer Name.
  # Setting this option is equivalent to running scutil --set ComputerName.
  # This name can contain spaces and Unicode characters.
  networking.computerName = "bw";

  # The hostname of your system, as visible from the command line and used by local and remote networks when connecting through SSH and Remote Login.
  # Setting this option is equivalent to running the command scutil --set HostName.
  # (Note that networking.localHostName defaults to the value of this option.)
  networking.hostName = "bw";

  # This seems like it can be used to remote deploy nix configs
  # This option lists the machines to be used if distributed builds are enabled (see nix.distributedBuilds). 
  # Nix will perform derivations on those machines via SSH by copying the inputs to the Nix store on the remote 
  # machine, starting the build, then copying the output back to the local Nix store.
  nix.buildMachines = [ ];

  # Automatically run the garbage collector at a specific time.
  nix.gc.automatic = true;

  # The calendar interval at which the garbage collector will run. 
  # See the serviceConfig.StartCalendarInterval option of the launchd module for more info.
  nix.gc.interval = [{
    Hour = 3;
    Minute = 15;
    Weekday = 7;
  }];

  # TODO: Does this conflict with auto-optimise-store?
  # Automatically run the nix store optimiser at a specific time.
  nix.optimise.automatic = true;

  # The calendar interval at which the optimiser will run. 
  # See the serviceConfig.StartCalendarInterval option of the launchd module for more info.
  nix.optimise.interval = [{
    Hour = 4;
    Minute = 15;
    Weekday = 7;
  }];

  # If set to true, Nix automatically detects files in the store that have identical contents, 
  # and replaces them with hard links to a single copy. This saves disk space. If set to false 
  # (the default), you can still run nix-store --optimise to get rid of duplicate files.
  nix.settings.auto-optimise-store = true;

  # If set, Nix will perform builds in a sandboxed environment that it will set up automatically 
  # for each build. This prevents impurities in builds by disallowing access to dependencies outside 
  # of the Nix store by using network and mount namespaces in a chroot environment. 
  # It doesn’t affect derivation hashes, so changing this option will not trigger a rebuild of packages.
  nix.settings.sandbox = true;

  # NOTE: I might want to add some things to this to allow some impurities with the build. Homebrew, maybe.
  # Directories from the host filesystem to be included in the sandbox.
  # nix.settings.extra-sandbox-paths = [];

  # Sets the mouse tracking speed. Found in the “Mouse” section of “System Preferences”. 
  # Set to -1.0 to disable mouse acceleration.
  system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = 1.0;

  # Sets the system-wide alert sound. Found under “Sound Effects” in the “Sound” section 
  # of “System Preferences”. Look in “/System/Library/Sounds” for possible candidates.
  system.defaults.".GlobalPreferences"."com.apple.sound.beep.sound" = null;

  # Sets custom system preferences
  system.defaults.CustomSystemPreferences = { };

  # Sets custom user preferences
  system.defaults.CustomUserPreferences = { };

  # NOTE: Are these different?
  # Enables swiping left or right with two fingers to navigate backward or forward. The default is true.
  system.defaults.NSGlobalDomain.AppleEnableMouseSwipeNavigateWithScrolls =
    false;

  # Enables swiping left or right with two fingers to navigate backward or forward. The default is true.
  system.defaults.NSGlobalDomain.AppleEnableSwipeNavigateWithScrolls = false;

  # Sets the level of font smoothing (sub-pixel font rendering).
  # Type: null or one of 0, 1, 2
  system.defaults.NSGlobalDomain.AppleFontSmoothing = 2;

  # Set to ‘Dark’ to enable dark mode, or leave unset for normal mode.
  system.defaults.NSGlobalDomain.AppleInterfaceStyle = "Dark";

  # Whether to automatically switch between light and dark mode. The default is false.
  system.defaults.NSGlobalDomain.AppleInterfaceStyleSwitchesAutomatically = false;

  # Whether to enable the press-and-hold feature. The default is true.
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = true;

  # Jump to the spot that’s clicked on the scroll bar. The default is false.
  system.defaults.NSGlobalDomain.AppleScrollerPagingBehavior = true;

  # Whether to show all file extensions in Finder. The default is false.
  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;

  # Whether to always show hidden files. The default is false.
  system.defaults.NSGlobalDomain.AppleShowAllFiles = true;

  # When to show the scrollbars. Options are ‘WhenScrolling’, ‘Automatic’ and ‘Always’.
  # Type: null or one of “WhenScrolling”, “Automatic”, “Always”
  system.defaults.NSGlobalDomain.AppleShowScrollBars = "Always";

  # If you press and hold certain keyboard keys when in a text area, 
  # the key’s character begins to repeat. For example, the Delete key continues 
  # to remove text for as long as you hold it down.
  # This sets how long you must hold down the key before it starts repeating.
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 0;

  # If you press and hold certain keyboard keys when in a text area, the key’s character begins to repeat. 
  # For example, the Delete key continues to remove text for as long as you hold it down.
  # This sets how fast it repeats once it starts.
  system.defaults.NSGlobalDomain.KeyRepeat = 0;

  # Whether to enable automatic capitalization. The default is true.
  system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;

  # Whether to enable smart dash substitution. The default is true.
  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;

  # Whether to enable inline predictive text. The default is true.
  system.defaults.NSGlobalDomain.NSAutomaticInlinePredictionEnabled = false;

  # Whether to enable smart period substitution. The default is true.
  system.defaults.NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;

  # Whether to enable smart quote substitution. The default is true.
  system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;

  # Whether to enable automatic spelling correction. The default is true.
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;

  # Whether to animate opening and closing of windows and popovers. The default is true.
  system.defaults.NSGlobalDomain.NSAutomaticWindowAnimationsEnabled = true;

  # Whether to save new documents to iCloud by default. The default is true.
  system.defaults.NSGlobalDomain.NSDocumentSaveNewDocumentsToCloud = false;

  # Whether to use expanded save panel by default. The default is false.
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;

  # LOL what?
  # Whether to use expanded save panel by default. The default is false.
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  # Whether to enable smooth scrolling. The default is true.
  system.defaults.NSGlobalDomain.NSScrollAnimationEnabled = true;

  # Whether to enable moving window by holding anywhere on it like on Linux. The default is false.
  system.defaults.NSGlobalDomain.NSWindowShouldDragOnGesture = true;

  # Whether to autohide the menu bar. The default is false.
  system.defaults.NSGlobalDomain._HIHideMenuBar = false;

  # Automatically install Mac OS software updates. Defaults to false.
  system.defaults.SoftwareUpdate.AutomaticallyInstallMacOSUpdates = false;

  # Enable Stage Manager Stage Manager arranges your recent windows into a single strip 
  # for reduced clutter and quick access. Default is false.
  system.defaults.WindowManager.GloballyEnabled = true;

  # Grouping strategy when showing windows from an application. false means “One at a time” true means “All at once”
  # system.defaults.WindowManager.AppWindowGroupingBehavior = true;

  # Click wallpaper to reveal desktop Clicking your wallpaper will move all 
  # windows out of the way to allow access to your desktop items and widgets. 
  # Default is true. false means “Only in Stage Manager” true means “Always”
  system.defaults.WindowManager.EnableStandardClickToShowDesktop = false;

  # Auto hide stage strip showing recent apps. Default is false.
  system.defaults.WindowManager.AutoHide = true;

  # Hide items in Stage Manager.
  system.defaults.WindowManager.HideDesktop = false;

  # Hide widgets in Stage Manager.
  system.defaults.WindowManager.StageManagerHideWidgets = false;

  # Hide items on desktop.
  system.defaults.WindowManager.StandardHideDesktopIcons = false;

  # Hide widgets on desktop.
  system.defaults.WindowManager.StandardHideWidgets = false;

  # Whether to automatically hide and show the dock. The default is false.
  system.defaults.dock.autohide = true;

  # Sets the speed of the autohide delay. The default is given in the example.
  system.defaults.dock.autohide-delay = 100.0; # 0.24

  # Whether to automatically rearrange spaces based on most recent use. The default is true.
  system.defaults.dock.mru-spaces = false;

  # Persistent applications in the dock.
  system.defaults.dock.persistent-apps = [ ];

  # Persistent folders in the dock.
  system.defaults.dock.persistent-others = [ ];

  # Show recent applications in the dock. The default is true.
  system.defaults.dock.show-recents = false;

  # Whether to show warnings when change the file extension of files. The default is true.
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  # Change the default finder view. “icnv” = Icon view, “Nlsv” = List view, “clmv” = Column View, “Flwv” = Gallery View The default is icnv.
  system.defaults.finder.FXPreferredViewStyle = "Nlsv";

  # Whether to allow quitting of the Finder. The default is false.
  system.defaults.finder.QuitMenuItem = true;

  # Show path breadcrumbs in finder windows. The default is false.
  system.defaults.finder.ShowPathbar = true;

  # Show status bar at bottom of finder windows with item/disk space stats. The default is false.
  system.defaults.finder.ShowStatusBar = true;

  # Show status bar at bottom of finder windows with item/disk space stats. The default is false.
  system.defaults.finder._FXShowPosixPathInTitle = true;

  # Disables the ability for a user to access the console by typing “>console” for a username at the login window. Default is false.
  system.defaults.loginwindow.DisableConsoleAccess = true;

  # Allow users to login to the machine as guests using the Guest account. Default is true.
  system.defaults.loginwindow.GuestEnabled = false;

  # Text to be shown on the login window. Default is “\\U03bb”.
  system.defaults.loginwindow.LoginwindowText = "🗝️";

  # Displays login window as a name and password field instead of a list of users. Default is false.
  system.defaults.loginwindow.SHOWFULLNAME = true;

  # The filesystem path to which screencaptures should be written.
  system.defaults.screencapture.location = "~/Captures";

  # Show thumbnail after screencapture before writing to file. The default is true.
  system.defaults.screencapture.show-thumbnail = false;

  # 0 to enable Silent Clicking, 1 to disable. The default is 1.
  system.defaults.trackpad.ActuationStrength = 0;

  # Whether to enable trackpad tap to click. The default is false.
  system.defaults.trackpad.Clicking = true;

  # 0 to disable three finger tap, 2 to trigger Look up & data detectors. The default is 2.
  system.defaults.trackpad.TrackpadThreeFingerTapGesture = 0;

  # Whether to enable the startup chime.
  # By default, this option does not affect your system configuration in any way. However, 
  # this means that after it has been set once, unsetting it will not return to the old behavior. 
  # It will allow the setting to be controlled in System Settings, though.
  system.startup.chime = false;

  # Every once in a while, a new NixOS release may change configuration defaults in a way incompatible 
  # with stateful data. For instance, if the default version of PostgreSQL changes, the new version will 
  # probably be unable to read your existing databases. To prevent such breakage, you can set the value 
  # of this option to the NixOS release with which you want to be compatible. The effect is that NixOS 
  # will option defaults corresponding to the specified release (such as using an older version of PostgreSQL).
  system.stateVersion = 4;

  # The time zone used when displaying times and dates. See https://en.wikipedia.org/wiki/List_of_tz_database_time_zones 
  # or run sudo systemsetup -listtimezones for a comprehensive list of possible values for this setting.
  time.timeZone = "America/New_York";

  programs.zsh.enable = true;
}
