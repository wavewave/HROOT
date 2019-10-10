{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: pkgs.callPackage ./nix/config.nix {} self super // {
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
    cp -a HROOT HROOT-core HROOT-graf HROOT-hist HROOT-io HROOT-math HROOT-tree HROOT-RooFit HROOT-RooFit-RooStats $out
  '';
}
