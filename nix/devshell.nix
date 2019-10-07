{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  config = import ./config.nix { inherit fetchgit; };

  newHaskellPackages = pkgs.haskellPackages.override { overrides = config; }; 

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            fficxx fficxx-runtime
            cmdargs  configurator
            containers directory filepath mtl
            process split stdenv template template-haskell transformers
            unordered-containers
          ]);
in

stdenv.mkDerivation {
  name = "HROOT-env";
  buildInputs = [ hsenv root ];
}
