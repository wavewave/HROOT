{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      callPackage ./nix/config.nix { } self super //
      callPackage ./default.nix { } self super;
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    HROOT-core
    HROOT-RooFit
    HROOT-RooFit-RooStats
  ]);

in

stdenv.mkDerivation {
  name = "HROOT-env";

  buildInputs = [
    root
    hsenv
  ];

}
