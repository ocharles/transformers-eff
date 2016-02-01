{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, transformers }:
      mkDerivation {
        pname = "effect-interpreters";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base transformers ];
        homepage = "https://github.com/ocharles/effect-interpreters";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
