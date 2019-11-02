{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  #fficxxSrc = fetchgit {
  #  url = "git://github.com/wavewave/fficxx.git";
  #  rev = "ac829cd8f315a4a7c1db13db9595f40dda0df7c8";
  #  sha256 = "1nz3hjx8jqsb200mcg7jm88p1004ka4h11wipzwcay87lxrsa4f1";
  #};
  fficxxSrc = /home/wavewave/repo/src/fficxx;

  newHaskellPackages0 = haskellPackages.override {
    overrides = callPackage ./nix/config.nix { inherit fficxxSrc; };
  };


  stdcxxNix = import ./nix/config-stdcxx.nix { inherit fficxxSrc stdenv fetchgit; packages = newHaskellPackages0; };
  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      callPackage ./nix/config.nix { inherit fficxxSrc; } self super //
      { "stdcxx" = self.callPackage stdcxxNix {}; };

  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    cabal2nix
    cabal-install
    #
    fficxx
    fficxx-runtime
    monad-loops
    stdcxx
  ]);

in

stdenv.mkDerivation {
  name = "HROOT-dev";

  buildInputs = [
    root
    hsenv
  ];

}
