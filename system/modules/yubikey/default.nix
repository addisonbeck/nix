
{pkgs, config, lib, ...}:
let
  homeDirectory = "/Users/${config.users.users.me.name}";
in
{
  # Install YubiKey packages
  home-manager.users.me = {
    home.packages = with pkgs; [
      yubikey-manager
      #yubioath-flutter # not building on mac
      pam_u2f
      gnupg
      pinentry-emacs
    ];

    # GPG Configuration
    programs.gpg = {
      enable = true;
      settings = {
        personal-cipher-preferences = "AES256 AES192 AES";
        personal-digest-preferences = "SHA512 SHA384 SHA256";
        default-preference-list = "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";
        cert-digest-algo = "SHA512";
        charset = "utf-8";
        keyid-format = "0xlong";
        with-fingerprint = true;
        use-agent = true;
      };
      scdaemonSettings = {
        disable-ccid = true;  # Prevent conflicts with pcscd
      };
    };

    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-emacs;
      defaultCacheTtl = 3600;
      maxCacheTtl = 86400;
    };

    # YubiKey Agent for sk-ed25519 keys
    # services.yubikey-agent.enable = true; # Not needed for sk-ed25519 keys
  };

  # System-level YubiKey support
  #services = {
  #  pcscd.enable = true;
  #  udev.packages = [ pkgs.yubikey-personalization ];
  #};
}
