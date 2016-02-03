{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, pipes, stdenv, transformers, criterion, free, mmorph }:
      mkDerivation {
        pname = "effect-interpreters";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base pipes transformers criterion free mmorph ];
        homepage = "https://github.com/ocharles/effect-interpreters";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
