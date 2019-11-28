{ pkgs ? import <nixpkgs> {}
, fficxxSrc ? (import ./nix/pinned.nix { inherit pkgs; }).fficxxSrc
}:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
    };
  };

  stdcxxNix = import (fficxxSrc + "/stdcxx-gen/default.nix") {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
      "stdcxx"         = self.callPackage stdcxxNix {};
    };

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
