{ pkgs ? import <nixpkgs> {}, fficxxSrc }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
    };
  };

  stdcxxNix = import ./nix/config-stdcxx.nix {
    inherit fficxxSrc stdenv fetchgit; packages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
      "stdcxx"         = self.callPackage stdcxxNix {};

      "HROOT-generate" = self.callCabal2nix "HROOT-generate" ./HROOT-generate { };
    };
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    HROOT-generate
  ]);

in

stdenv.mkDerivation {
  name = "HROOT-src";
  buildInputs = [
    hsenv
    root
  ];
  src = ./.;
  buildPhase = ''
    HROOT-generate
  '';
  installPhase = ''
    mkdir -p $out
    cp -a HROOT HROOT-core HROOT-graf HROOT-hist HROOT-io HROOT-math HROOT-net HROOT-tree HROOT-RooFit HROOT-RooFit-RooStats $out
  '';
}
