{ mkDerivation, base, hpack, parsec, stdenv, tasty, tasty-hunit
, text
}:
mkDerivation {
  pname = "lang";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base parsec text ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base parsec tasty tasty-hunit text ];
  preConfigure = "hpack";
  homepage = "https://github.com/jgertm/lang#readme";
  license = stdenv.lib.licenses.bsd3;
}
