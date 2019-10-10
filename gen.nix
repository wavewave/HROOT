{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: pkgs.callPackage ./nix/config.nix {} self super // {
      "HROOT-generate" = self.callCabal2nix "HROOT-generate" ./HROOT-generate { };
    };
  };

in

newHaskellPackages.HROOT-generate
