{
  description = "Haskell 'sr' library";

  inputs = {
    flakety.url = "github:k0001/flakety";
    nixpkgs.follows = "flakety/nixpkgs";
    flake-parts.follows = "flakety/flake-parts";

    hs_kind.url = "github:k0001/hs-kind";
    hs_kind.inputs.flakety.follows = "flakety";

    hs_leb128-binary.url = "gitlab:k0001/leb128-binary";
    hs_leb128-binary.inputs.nixpkgs.follows = "nixpkgs";
    hs_leb128-binary.inputs.flake-parts.follows = "flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.hs_kind.overlays.default
        #inputs.hs_leb128-binary.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = prev.lib.composeExtensions
              (prev.haskell.packageOverrides or (_: _: { }))
              (hself: hsuper: { sr = hself.callPackage ./sr { }; });
          };
        })
      ];
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          sr__ghc96 = pkgs.haskell.packages.ghc96.sr;
          sr__ghc98 = pkgs.haskell.packages.ghc98.sr;
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.sr__ghc96
              config.packages.sr__ghc96.doc
              config.devShells.ghc96
              config.packages.sr__ghc98
              config.packages.sr__ghc98.doc
              config.devShells.ghc98
            ];
          };
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.sr ];
              withHoogle = false;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc98;
          ghc96 = mkShellFor pkgs.haskell.packages.ghc96;
          ghc98 = mkShellFor pkgs.haskell.packages.ghc98;
        };
      };
    };
}
