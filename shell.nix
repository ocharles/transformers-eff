{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, free, list-transformer, mmorph, stdenv
      , transformers
      }:
      mkDerivation {
        pname = "transformers-eff";
        version = "0.2.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base free list-transformer mmorph transformers
        ];
        homepage = "https://github.com/ocharles/transformers-eff";
        description = "An approach to managing composable effects, ala mtl/transformers/extensible-effects/Eff";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
