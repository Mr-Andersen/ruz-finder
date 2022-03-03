let
  pkgs = import <nixpkgs> {};
  inherit (pkgs) dockerTools;

  project = import ./.;
  inherit (project) ruz-finder;
  inherit (ruz-finder.identifier) name version;
  ruz-finder-bin = ruz-finder.components.exes.ruz-finder;
in dockerTools.buildImage {
  inherit name version;
  contents = [ ruz-finder-bin ];
  config = {
    Entrypoint = [ "${ruz-finder-bin}/bin/ruz-finder" ];
  };
}
