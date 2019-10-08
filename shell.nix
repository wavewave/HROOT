{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  newHaskellPackages = haskellPackages.override {
    overrides = callPackage ./nix/config.nix { };
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    cabal2nix
    cabal-install
    #
    configurator
    fficxx
    fficxx-runtime
  ]);

in

stdenv.mkDerivation {
  name = "HROOT-dev";

  buildInputs = [ #root
                  hsenv ];

}
