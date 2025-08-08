{
  inputs,
  pkgs,
  ...
}: {
  # Avoids a logout/login cycle
  #system.activationScripts.postUserActivation.text = ''
    #/System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  #'';

  # Turns on key mapping
  system.keyboard.enableKeyMapping = true;

  # Remaps caps lock to escape
  system.keyboard.remapCapsLockToControl = true;

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

  # List of additional package outputs to be symlinked into /run/current-systemsandboxed environment that it will set up automatically
  # for each build. This prevents impurities in builds by disallowing access to dependencies outside
  # of the Nix store by using network and mount namespaces in a chroot environment.
  # It doesn’t affect derivation hashes, so changing this option will not trigger a rebuild of packages.
  nix.settings.sandbox = false;

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
  system.defaults.CustomSystemPreferences = {};

  # Sets custom user preferences
  system.defaults.CustomUserPreferences = {};

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
  system.defaults.dock.persistent-apps = [];

  # Persistent folders in the dock.
  system.defaults.dock.persistent-others = [];

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

  # A list of permissible login shells for user accounts. No need to mention /bin/sh and other shells that are available by default on macOS.
  programs.zsh.enable = true;
  #environment.loginShell = pkgs.zsh;
  environment.shells = [pkgs.zsh];
  system.primaryUser = "me";
}
