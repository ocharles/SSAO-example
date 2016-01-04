{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, distributive, gl, lens
      , linear, random, sdl2, stdenv, text, transformers, JuicyPixels
      }:
      mkDerivation {
        pname = "ssao-example";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          attoparsec base distributive gl lens linear random sdl2 text
          transformers JuicyPixels
        ];
        homepage = "https://github.com/ocharles/ssao-example";
        description = "A demonstration of screen-space ambient occlusion using OpenGL & Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
