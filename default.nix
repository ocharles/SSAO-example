{ mkDerivation, attoparsec, base, distributive, gl, lens, linear
, random, sdl2, stdenv, text, transformers
}:
mkDerivation {
  pname = "ssao-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base distributive gl lens linear random sdl2 text
    transformers
  ];
  homepage = "https://github.com/ocharles/ssao-example";
  description = "A demonstration of screen-space ambient occlusion using OpenGL & Haskell";
  license = stdenv.lib.licenses.bsd3;
}
