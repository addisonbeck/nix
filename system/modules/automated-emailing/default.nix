{ pkgs, lib, config, ... }: let
  isDarwin = pkgs.stdenv.isDarwin;
  calibreBinary = if isDarwin then
    "/opt/homebrew/bin/ebook-convert"
  else
    "${pkgs.calibre}/bin/ebook-convert";
  orgRoamFindFileEl = ./org-roam-find-file.py;
  cookbook-css = ./cookbook.css;
  orgRoamFindNodePy = ./org-roam-find-node-file.py;
  memory-css = ./memory.css;
in {
  options.my.kindle-send = lib.mkOption {
    type = lib.types.package;
    default = pkgs.writeShellScriptBin "kindle-send" (builtins.readFile (pkgs.replaceVars ./kindle-send.sh {
      mutt = "${pkgs.mutt}/bin/mutt";
      zip = "${pkgs.zip}/bin/zip";
      muttrc = "${config.environment.etc."Muttrc".source}";
      sendTo = "us+amazon_WGMxDE@kindle.com";
      epubcheck = "${pkgs.epubcheck}/bin/epubcheck";
    }));
    description = "The kindle-send script for emailing EPUBs to a Kindle address.";
    readOnly = true;
  };
  options.my.wikipedia-to-kindle-generate = lib.mkOption {
    type = lib.types.package;
    default = pkgs.writeShellScriptBin "wikipedia-to-kindle-generate"
    (builtins.readFile (pkgs.replaceVars ./wikipedia-to-kindle-generate.sh {
        bash = "${pkgs.bash}/bin/bash";
        kindle-send = "${config.my.kindle-send}/bin/kindle-send";
        ebook-convert = "${calibreBinary}";
    }));
    description = "The kindle-send script for emailing EPUBs to a Kindle address.";
    readOnly = true;
  };
  options.my.cookbook-to-kindle-generate = lib.mkOption {
    type = lib.types.package;
    default = pkgs.writeShellScriptBin "cookbook-to-kindle-generate"
    (builtins.readFile (pkgs.replaceVars ./cookbook-to-kindle-generate.sh {
        bash = "${pkgs.bash}/bin/bash";
        kindle-send = "${config.my.kindle-send}/bin/kindle-send";
        org-roam-find-file = "${orgRoamFindFileEl}";
        pandoc = "${pkgs.pandoc}/bin/pandoc";
        cookbook-css = "${cookbook-css}";
    }));
    description = "Generates an epub from my org roam cookbook and sends it to my kindle";
    readOnly = true;
  };
  options.my.memory-to-kindle-generate = lib.mkOption {
    type = lib.types.package;
    default = pkgs.writeShellScriptBin "memory-to-kindle-generate"
      (builtins.readFile (pkgs.replaceVars ./memory-to-kindle-generate.sh {
        bash = "${pkgs.bash}/bin/bash";
        kindle-send = "${config.my.kindle-send}/bin/kindle-send";
        org-roam-find-node-file = "${orgRoamFindNodePy}";
        pandoc = "${pkgs.pandoc}/bin/pandoc";
        memory-css = "${memory-css}";
        python3 = "${pkgs.python3}/bin/python3";
      }));
    description = "Generate an EPUB from a single Org-roam memory (by ID or file) and send it to Kindle.";
    readOnly = true;
  };
  config = {
    age.secrets.do-not-reply-email-password.file = ../../with/user/with/secret/do-not-reply-email-password.age;
    environment.systemPackages = [
      pkgs.mutt
      pkgs.msmtp
      pkgs.zip
      config.my.kindle-send
      config.my.wikipedia-to-kindle-generate
      config.my.cookbook-to-kindle-generate
      config.my.memory-to-kindle-generate
    ] ++ lib.optionals (!isDarwin) [
      pkgs.calibre
    ];
    environment.etc."wikipedia.recipe".text = builtins.readFile ./wikipedia-recipe.py;
    environment.etc."Muttrc".text = ''
        set ssl_starttls=yes
        set ssl_force_tls=yes
        set imap_user = "do-not-reply@addisonbeck.com"
        set imap_pass = "`cat ${config.age.secrets.do-not-reply-email-password.path}`"
        set from = "do-not-reply@addisonbeck.com"
        set realname = "RSS Bot"
        set folder = "imaps://box.addisonbeck.com:993"
        set spoolfile = "+INBOX"
        set smtp_url = "smtps://do-not-reply@addisonbeck.com@box.addisonbeck.com:465/"
        set smtp_pass = "`cat ${config.age.secrets.do-not-reply-email-password.path}`"
        set ssl_verify_dates = yes
        # vim:ft=muttrc
    '';
  }; 
}
