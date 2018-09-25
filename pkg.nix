{ mkDerivation, aeson, attoparsec, base, bytestring, pipes
, pipes-attoparsec, pipes-bytestring, pipes-parse, stdenv
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
  license = stdenv.lib.licenses.bsd3;
}
