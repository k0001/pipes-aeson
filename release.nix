{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? (import nixpkgsBootstrap {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "febc24b11ac5369734f81225dc8a200e7dc75319"; # haskell-updates 2022-03-21
    sha256 = "sha256-JTYxQTU34Xs8tK6fdIWVNOcaAS0p683DknIMq8FnfGg="; }
}:

let

pkgs = import nixpkgs {};

hsPackageSetConfig = self: super: {
  pipes-aeson = self.callPackage (import ./pkg.nix) {};
};

hs = pkgs.haskell.packages.ghc902.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (hs) pipes-aeson; }
