{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, stacklock2nix, all-cabal-hashes }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgs = system: import nixpkgs {
        inherit system;
        overlays = [
          stacklock2nix.overlay
          self.overlays.default
        ];
      };
    in
    {
      overlays.default = final: prev: {
        ruz-finder = final.ruz-finder-pkgSet.pkgSet.ruz-finder;
        ruz-finder-pkgSet = final.stacklock2nix {
          stackYaml = ./stack.yaml;
          stackYamlLock = ./stack.yaml.lock;
          baseHaskellPkgSet = final.haskell.packages.ghc964;
          additionalHaskellPkgSetOverrides = hfinal: hprev: {
            mkDerivation = a: hprev.mkDerivation (a // { doCheck = false; doHaddock = false; });
          };
          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet: [
            final.cabal-install
            final.haskell.packages.ghc964.haskell-language-server
          ];
          inherit all-cabal-hashes;
        };
      };
      nixosModules.default = import ./nix/module.nix self;
      packages = forAllSystems (system: {
        default = (pkgs system).ruz-finder;
      });
      devShells = forAllSystems (system: {
        default = (pkgs system).ruz-finder-pkgSet.devShell.overrideAttrs (a: {
          shellHook =
            (a.shellHook or "") +
            ''
              export NIX_PATH=nixpkgs=${nixpkgs}
            '';
        });
        stack = (pkgs system).mkShell {
          packages = [ (pkgs system).stack ];
          shellHook =
            ''
              export NIX_PATH=nixpkgs=${nixpkgs}
            '';
        };
      });
      formatter = forAllSystems (system:
        let
          inherit (pkgs system) fd haskell nixpkgs-fmt writeScriptBin;
          inherit (haskell.packages.ghc964) fourmolu;
        in
        writeScriptBin "fourmolu-inline" ''
          set -ex
          ${fourmolu}/bin/fourmolu -i `${fd}/bin/fd -g *.hs src`
          ${nixpkgs-fmt}/bin/nixpkgs-fmt `${fd}/bin/fd -g *.nix .`
        '');
      legacyPackages = forAllSystems pkgs;
    };
}
