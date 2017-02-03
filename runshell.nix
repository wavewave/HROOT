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
  shellHook = ''
    PS1="\n\[\033[0;34m\][\u@\h.HROOT-generate:\w]\$\[\033[0m\] "
  '';
}
