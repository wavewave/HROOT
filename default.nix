{ pkgs }:

let

  HROOT-src = pkgs.callPackage ./gen.nix { };

in self: super:

{
  "HROOT" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT" (HROOT-src + "/HROOT") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-core" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-core" (HROOT-src + "/HROOT-core") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-graf" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-graf" (HROOT-src + "/HROOT-graf") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-hist" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-hist" (HROOT-src + "/HROOT-hist") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-io" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-io" (HROOT-src + "/HROOT-io") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-math" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-math" (HROOT-src + "/HROOT-math") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-net" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-net" (HROOT-src + "/HROOT-net") {
      RHTTP = null;
    }) { librarySystemDepends = [ pkgs.root ]; };
  "HROOT-tree" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-tree" (HROOT-src + "/HROOT-tree") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-RooFit" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-RooFit" (HROOT-src + "/HROOT-RooFit") { }) {
      librarySystemDepends = [ pkgs.root ];
    };
  "HROOT-RooFit-RooStats" = pkgs.haskell.lib.overrideCabal
    (self.callCabal2nix "HROOT-RooFit-RooStats"
      (HROOT-src + "/HROOT-RooFit-RooStats")
      { }) { librarySystemDepends = [ pkgs.root ]; };
}
