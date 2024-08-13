{
  lib,
  pkgs,
  config,
  ...
}: 
let
  cfg = config.service.autoclone;

  cloneScript = "${pkgs.writeShellScript "cloneNotes" ''
    export GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh -i ${cfg.ssh-key} -o IdentitiesOnly=yes"
    ${pkgs.git}/bin/git clone ${cfg.url} ${cfg.save-path}
  ''}";
in
{
  options = {
    service.autoclone = {
      enable = lib.mkEnableOption "autoclone";
      name = lib.mkOption {
        type = lib.types.str;
        description = "The name of the repository";
      };
      url = lib.mkOption {
        type = lib.types.str;
        description = "The url to clone from";
      };
      save-path = lib.mkOption {
       type = lib.types.str;
       description = "The path to save the repo to on the file system";
      };
      ssh-key = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "ssh-key used to pull the repository";
      };
    };
  };
  config = lib.mkIf cfg.enable {
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
    launchd.agents.autoclone = lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      enable = true;
      config = {
	Label = "Clone notes";
	Program = "${cloneScript}";
	ProcessType = "Background";
	RunAtLoad = true;
	# EnvironmentVariables
      };
    };
  };
}
