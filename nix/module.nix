self:
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkOption types;
in
{
  options.services.ruz-finder-bot = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    tg-token = mkOption {
      type = types.str;
    };
    vk-token = mkOption {
      type = types.str;
    };
  };

  config =
    let cfg = config.services.ruz-finder-bot;
      in mkIf cfg.enable
    {
      systemd.services.ruz-finder-bot = {
        enable = true;
        environment = {
          VK_TOKEN = cfg.vk-token;
          TG_TOKEN = cfg.tg-token;
        };
        script = ''
          ${self.packages.${pkgs.system}.default}/bin/ruz-finder bot
        '';
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Restart = "always";
        };
      };
    };
}