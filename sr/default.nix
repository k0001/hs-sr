{ mkDerivation, aeson, base, binary, deepseq, hashable, hedgehog
, integer-logarithms, kind-rational, leb128-binary, lib, scientific
, tasty, tasty-hedgehog
}:
mkDerivation {
  pname = "sr";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary deepseq hashable hedgehog integer-logarithms
    kind-rational leb128-binary scientific
  ];
  testHaskellDepends = [
    aeson base binary hedgehog scientific tasty tasty-hedgehog
  ];
  homepage = "https://gitlab.com/k0001/hs-sr";
  description = "Numbers safely representable as both Scientific and Rational";
  license = lib.licenses.bsd3;
}
