{ mkDerivation, base, containers, generic-lens, generic-lens-labels
, haskeline, hpack, microlens-platform, mtl, parsec, pretty-simple
, stdenv, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers generic-lens generic-lens-labels microlens-platform
    mtl parsec pretty-simple text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base haskeline text ];
  testHaskellDepends = [ base tasty tasty-hunit text ];
  preConfigure = "hpack";
  homepage = "https://github.com/jgertm/lang#readme";
  license = stdenv.lib.licenses.bsd3;
}
