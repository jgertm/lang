{ mkDerivation, base, criterion, deepseq, doctest, HUnit
, inspection-testing, lens, profunctors, QuickCheck, stdenv, tagged
}:
mkDerivation {
  pname = "generic-lens";
  version = "1.0.0.2";
  sha256 = "5e6f8a188e84b0130c55d469d6bb379ac323fb59008fd84eaf73360bb8934168";
  libraryHaskellDepends = [ base profunctors tagged ];
  testHaskellDepends = [
    base doctest HUnit inspection-testing lens profunctors
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq lens QuickCheck
  ];
  homepage = "https://github.com/kcsongor/generic-lens";
  description = "Generically derive traversals, lenses and prisms";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
