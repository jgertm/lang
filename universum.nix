{ mkDerivation, base, bytestring, containers, deepseq, doctest
, gauge, ghc-prim, Glob, hashable, hedgehog, microlens
, microlens-mtl, mtl, safe-exceptions, stdenv, stm, tasty
, tasty-hedgehog, text, transformers, type-operators
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "universum";
  version = "1.2.0";
  sha256 = "37a10c85d0b4812a9687846cdfe20a61eb102839318149ad036d8c1be47e8518";
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim hashable microlens
    microlens-mtl mtl safe-exceptions stm text transformers
    type-operators unordered-containers utf8-string vector
  ];
  testHaskellDepends = [
    base bytestring doctest Glob hedgehog tasty tasty-hedgehog text
    utf8-string
  ];
  benchmarkHaskellDepends = [
    base containers gauge unordered-containers
  ];
  homepage = "https://github.com/serokell/universum";
  description = "Custom prelude used in Serokell";
  license = stdenv.lib.licenses.mit;
}
