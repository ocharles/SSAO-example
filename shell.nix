{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, distributive, OpenGLRaw, lens
      , linear, random, sdl2, stdenv, text, transformers, JuicyPixels
      }:
      mkDerivation {
        pname = "ssao-example";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          attoparsec base distributive OpenGLRaw lens linear random sdl2 text
          transformers JuicyPixels
        ];
        homepage = "https://github.com/ocharles/ssao-example";
        description = "A demonstration of screen-space ambient occlusion using OpenGL & Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = with pkgs; (haskellPackages.override {
    overrides = self: super: {
      OpenGL = self.callPackage
        ({ mkDerivation, base, bytestring, GLURaw, ObjectName, OpenGLRaw, StateVar, text }:
        mkDerivation {
          pname = "OpenGL";
          version = "3.0.0.0";
          sha256 = "1w2vn2xjnr7cciknlvr4486n2hi82jdacni9kwvkgn7y02w7cnph";
          libraryHaskellDepends = [ base bytestring GLURaw ObjectName OpenGLRaw StateVar text ];
          homepage = "http://www.haskell.org/haskellwiki/Opengl";
          description = "A binding for the OpenGL graphics system";
          license = stdenv.lib.licenses.bsd3;
          hydraPlatforms = stdenv.lib.platforms.none;
        }) {};

      OpenGLRaw = self.callPackage
        ({ mkDerivation, base, fixed, half, ghc-prim, mesa, text }:
        mkDerivation {
          pname = "OpenGLRaw";
          version = "3.0.0.0";
          sha256 = "1wdbisgjsajlpq622ap9n0h4dc92wgimjnzrfylwbpdr1g4c9vw1";
          libraryHaskellDepends = [ base ghc-prim fixed half text ];
          librarySystemDepends = [ mesa ];
          homepage = "http://www.haskell.org/haskellwiki/Opengl";
          description = "A raw binding for the OpenGL graphics system";
          license = stdenv.lib.licenses.bsd3;
          hydraPlatforms = stdenv.lib.platforms.none;
        }) {inherit (pkgs) mesa;};

      GLURaw = self.callPackage
        ({ mkDerivation, base, freeglut, mesa, OpenGLRaw }:
        mkDerivation {
          pname = "GLURaw";
          version = "2.0.0.0";
          sha256 = "014i3mi66yc2g1yik3wfynh6mxkzw75xrlkcdim8yrmnr8dmvpcd";
          libraryHaskellDepends = [ base OpenGLRaw ];
          librarySystemDepends = [ freeglut mesa ];
          homepage = "http://www.haskell.org/haskellwiki/Opengl";
          description = "A raw binding for the OpenGL graphics system";
          license = stdenv.lib.licenses.bsd3;
          hydraPlatforms = stdenv.lib.platforms.none;
        }) {inherit (pkgs) freeglut; inherit (pkgs) mesa;};
      };
  }).callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
