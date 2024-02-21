{
  description = "Haskell 'pipes-aeson' library";

  inputs = {
    flakety.url = "github:k0001/flakety";
    nixpkgs.follows = "flakety/nixpkgs";
    flake-parts.follows = "flakety/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.flakety.overlays.default
        (final: prev:
          let
            hsLib = prev.haskell.lib;
            hsClean = drv:
              hsLib.overrideCabal drv
              (old: { src = prev.lib.sources.cleanSource old.src; });
          in {
            haskell = prev.haskell // {
              packageOverrides = prev.lib.composeExtensions
                (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
                  pipes-aeson = hsClean (hself.callPackage ./pipes-aeson { });
                });
            };
          })
      ];
      systems = [ "x86_64-linux" ];
      perSystem = { config, system, pkgs, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.devShells.ghc98
              config.packages.pipes-aeson__ghc98
              config.packages.pipes-aeson__ghc98.doc
              config.packages.pipes-aeson__ghc98__sdist
              config.packages.pipes-aeson__ghc98__sdistDoc
            ];
          };

          pipes-aeson__ghc98 = pkgs.haskell.packages.ghc98.pipes-aeson;
          pipes-aeson__ghc98__sdist =
            pkgs.haskell.packages.ghc98.cabalSdist { src = ./pipes-aeson; };
          pipes-aeson__ghc98__sdistDoc = pkgs.haskell.lib.documentationTarball
            config.packages.pipes-aeson__ghc98;
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.pipes-aeson ];
              withHoogle = true;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc98;
          ghc98 = mkShellFor pkgs.haskell.packages.ghc98;
        };
      };
    };
}
