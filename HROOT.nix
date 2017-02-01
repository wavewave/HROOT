{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  config = import ./config.nix { inherit fetchgit; };
  #HROOTsrc =
  #  let
  #
  #  in 
  #HROOTconfig = self: super: {
  #
  #};

  newHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: config self super // HROOTconfig self super;
  }; 

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            HROOT-generate fficxx fficxx-runtime
          ]);
in

stdenv.mkDerivation {
  name = "HROOT-env";
  buildInputs = [ hsenv root ];
}
