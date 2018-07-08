{ mkDerivation, base, hpack, parsec, stdenv, text }:
mkDerivation {
  pname = "lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base parsec text ];
  preConfigure = "hpack";
  homepage = "https://github.com/jgertm/lang#readme";
  license = stdenv.lib.licenses.bsd3;
}
