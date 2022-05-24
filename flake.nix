{
  description = "HROOT";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, fficxx }:
    let
      pkgs = import nixpkgs {
        overlays = [ fficxx.overlay ];
        system = "x86_64-linux";
      };

      finalHaskellOverlay = self: super:
        (import ./default.nix { inherit pkgs; } self super);

      newHaskellPackages = pkgs.haskellPackages.extend finalHaskellOverlay;

    in {
      packages.x86_64-linux = {
        inherit (newHaskellPackages)
          HROOT HROOT-core HROOT-graf HROOT-hist HROOT-io HROOT-math HROOT-net
          HROOT-tree HROOT-RooFit HROOT-RooFit-RooStats;
      };

      # see these issues and discussions:
      # - https://github.com/NixOS/nixpkgs/issues/16394
      # - https://github.com/NixOS/nixpkgs/issues/25887
      # - https://github.com/NixOS/nixpkgs/issues/26561
      # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            finalHaskellOverlay;
        });
      };

      devShells.x86_64-linux = {
        "default" = let
          hsenv = pkgs.haskellPackages.ghcWithPackages
            (p: [ p.cabal-install p.fficxx p.fficxx-runtime p.stdcxx ]);
        in pkgs.mkShell {
          buildInputs = [ hsenv pkgs.root ];
          shellHook = "";
        };
        "codevelop" = pkgs.haskellPackages.shellFor {
          packages = ps: [ ps.fficxx ps.fficxx-runtime ];
          buildInputs = [ pkgs.cabal-install pkgs.root ];
          withHoogle = false;
        };

      };
    };
}
