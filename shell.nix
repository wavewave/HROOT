{ pkgs ? import <nixpkgs> {}
, fficxxSrc ?
    pkgs.fetchgit {
      url = "git://github.com/wavewave/fficxx.git";
      rev = "21487f389b9f88cad6f506fb0db06534ed96bc28";
      sha256 = "1b8plcpawd2dv0vy0dzy4dms0afm5dc09yxxgk68x2ghqb1cd0wf";
    }
}:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = callPackage ./nix/config.nix { inherit fficxxSrc; };
  };

  stdcxxNix = import ./nix/config-stdcxx.nix {
    inherit fficxxSrc stdenv fetchgit; packages = newHaskellPackages0;
  };

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
