{ mkDerivation, base-noprelude, containers, generic-lens
, generic-lens-labels, haskeline, hpack, microlens-platform, mtl
, parsec, pretty-simple, stdenv, tasty, tasty-hunit, text
, universum
}:
mkDerivation {
  pname = "lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base-noprelude containers generic-lens generic-lens-labels
    microlens-platform mtl parsec pretty-simple text universum
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base-noprelude haskeline text ];
  testHaskellDepends = [ base-noprelude tasty tasty-hunit text ];
  preConfigure = "hpack";
  homepage = "https://github.com/jgertm/lang#readme";
  license = stdenv.lib.licenses.bsd3;
}
