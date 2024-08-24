{ lib, pkgs, config, ... }:
let
  cfg = config.services.autoclone;
  autoclone-type = with lib.types;
    attrsOf (submodule ({ name, ... }: {
      options = {
        enable = lib.mkEnableOption "autoclone repo";

        service-name = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "A name for the service";
        };

        url = lib.mkOption {
          type = lib.types.str;
          description = "Url to clone";
        };

        save-path = lib.mkOption {
          type = lib.types.path;
          description = "Path to save to";
        };

        ssh-key = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "SSH key used to clone the repository";
        };
      };
    }));
in {
  options = {
    services.autoclone = {
      enable = lib.mkEnableOption "autoclone";
      repo = lib.mkOption { type = autoclone-type; };
    };
  };
  config = let repos = lib.filterAttrs (_: { enable, ... }: enable) cfg.repo;
  in lib.mkIf cfg.enable {
    # systemd.user.enable = true;
    # systemd.user.startServices = "sd-switch";
    # systemd.user.services.autoclone = {
    #   Unit = {
    #     Description = "Automatically clone ${cfg.name}";
    #     StartLimitIntervalSec = 0;
    #     StartLimitBurst = 3;
    #     OnBootSec = 5;
    #     OnUnitActiveSec = 5;
    #     After = [ "agenix.service" "network.target" "network-online.target"];
    #     Requires = [ "agenix.service" ];
    #   };
    #   Install = {
    #     WantedBy = [ "default.target" ];
    #   };
    #   Service = {
    #     Type = "oneshot";
    #     ExecStart = "${pkgs.writeShellScript "cloneRepos" ''
    #       export GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh -i ${cfg.ssh-key} -o IdentitiesOnly=yes"
    #       ${pkgs.git}/bin/git clone ${cfg.url} ${cfg.save-path}
    #       if command -v direnv
    #       then
    #         direnv allow ${cfg.save-path}
    #       fi
    #     ''}";
    #   };
    # };
    launchd.agents =
      lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin lib.mapAttrs' (_:
        { service-name, url, save-path, ssh-key, ... }:
        lib.nameValuePair "autoclone@${service-name}" {
          enable = true;
          config = {
            Label = "clone-${service-name}";
            Program = "${pkgs.writeShellScript "clone-${service-name}" ''
              export GIT_SSH_COMMAND=\"${pkgs.openssh}/bin/ssh -i ${ssh-key} -o IdentitiesOnly=yes\"
              ${pkgs.git}/bin/git clone ${url} ${save-path}
            ''}";
            ProcessType = "Background";
            RunAtLoad = true;
          };
        }) repos;
  };
}
