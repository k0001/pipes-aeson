{ mkDerivation, aeson, attoparsec, base, bytestring, pipes
, pipes-attoparsec, pipes-bytestring, pipes-parse, lib
, transformers
}:
mkDerivation {
  pname = "pipes-aeson";
  version = "0.4.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring pipes pipes-attoparsec
    pipes-bytestring pipes-parse transformers
  ];
  homepage = "https://github.com/k0001/pipes-aeson";
  description = "Encode and decode JSON streams using Aeson and Pipes";
  license = lib.licenses.bsd3;
}
