{ mkDerivation, ansi-terminal, base-noprelude, containers
, data-default, data-partition, generic-lens, haskeline, hpack
, microlens-platform, mtl, optparse-applicative, parsec
, pretty-simple, prettyprinter, stdenv, tasty, tasty-hunit, text
, unification-fd, universum
}:
mkDerivation {
  pname = "lang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base-noprelude containers data-default data-partition
    generic-lens microlens-platform mtl parsec pretty-simple
    prettyprinter text unification-fd universum
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    ansi-terminal base-noprelude containers haskeline
    optparse-applicative parsec text
  ];
  testHaskellDepends = [
    ansi-terminal base-noprelude containers parsec tasty tasty-hunit
    text
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/jgertm/lang#readme";
  license = stdenv.lib.licenses.bsd3;
}
