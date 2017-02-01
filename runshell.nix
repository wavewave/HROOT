{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  fficxxSrc = (import ./config.nix { inherit fetchgit;}).fficxxSrc;

  config = import ./config.nix { inherit fetchgit; };

  newHaskellPackages = pkgs.haskellPackages.override { overrides = config; }; 

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            HROOT-generate fficxx fficxx-runtime
          ]);

in

stdenv.mkDerivation {
  name = "HROOT-env";
  buildInputs = [ hsenv root ];
}
