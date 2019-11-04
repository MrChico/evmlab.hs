{ mkDerivation, aeson, base, bytestring, process, QuickCheck
, stdenv, tasty, tasty-quickcheck, text
}:
mkDerivation {
  pname = "evmlab";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring process QuickCheck tasty tasty-quickcheck
    text
  ];
  license = stdenv.lib.licenses.agpl3;
}
