{ pkgs, root }:

hself: hsuper:

let

  HROOT-src = (pkgs.callPackage ./gen.nix { }) hself;

in rec {
  "HROOT-generate" = hself.callCabal2nix "HROOT-generate" ./HROOT-generate { };
  "HROOT" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT" (HROOT-src + "/HROOT") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-core" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-core" (HROOT-src + "/HROOT-core") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-graf" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-graf" (HROOT-src + "/HROOT-graf") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-hist" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-hist" (HROOT-src + "/HROOT-hist") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-io" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-io" (HROOT-src + "/HROOT-io") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-math" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-math" (HROOT-src + "/HROOT-math") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-net" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-net" (HROOT-src + "/HROOT-net") {
      RHTTP = null;
    }) { librarySystemDepends = [ root ]; };
  "HROOT-tree" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-tree" (HROOT-src + "/HROOT-tree") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-RooFit" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-RooFit" (HROOT-src + "/HROOT-RooFit") { }) {
      librarySystemDepends = [ root ];
    };
  "HROOT-RooFit-RooStats" = pkgs.haskell.lib.overrideCabal
    (hself.callCabal2nix "HROOT-RooFit-RooStats"
      (HROOT-src + "/HROOT-RooFit-RooStats")
      { }) { librarySystemDepends = [ root ]; };
}
