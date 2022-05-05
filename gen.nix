{ pkgs }:

with pkgs;

let

  # see these issues and discussions:
  # - https://github.com/NixOS/nixpkgs/issues/16394
  # - https://github.com/NixOS/nixpkgs/issues/25887
  # - https://github.com/NixOS/nixpkgs/issues/26561
  # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
  newHaskellPackages = pkgs.haskellPackages.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: {
        "HROOT-generate" =
          self.callCabal2nix "HROOT-generate" ./HROOT-generate { };
      });
  });

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [ HROOT-generate ]);

in stdenv.mkDerivation {
  name = "HROOT-src";
  buildInputs = [ hsenv root ];
  src = ./.;
  buildPhase = ''
    HROOT-generate
  '';
  installPhase = ''
    mkdir -p $out
    cp -a HROOT HROOT-core HROOT-graf HROOT-hist HROOT-io HROOT-math HROOT-net HROOT-tree HROOT-RooFit HROOT-RooFit-RooStats $out
  '';
}
