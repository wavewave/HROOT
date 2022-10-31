{
  description = "HROOT";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, fficxx }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        root = if system == "aarch64-darwin" then
          (pkgs.root.override { tbb = null; }).overrideAttrs (old: rec {
            cmakeFlags = [ "-Dbuiltin_tbb=OFF" ]
              ++ (builtins.map (x: if (x == "-Dimt=ON") then "-Dimt=OFF" else x)
                old.cmakeFlags);
            preBuild = ''
              export MACOSX_DEPLOYMENT_TARGET="10.16"
              echo "PRE BUILD HOOK IS CALLED"
            '';
          })
        else
          pkgs.root;

        haskellOverlay = final: self: super:
          (import ./default.nix { pkgs = final; } self super);

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            (fficxx.haskellOverlay.${system} pkgs hself hsuper
              // haskellOverlay pkgs hself hsuper));

        mkPackages = compiler: {
          inherit (hpkgsFor compiler)
            HROOT HROOT-core HROOT-graf HROOT-hist HROOT-io HROOT-math HROOT-net
            HROOT-tree HROOT-RooFit HROOT-RooFit-RooStats;
        };

        mkShellFor = compiler:
          let
            hsenv = (hpkgsFor compiler).ghcWithPackages
              (p: [ p.cabal-install p.fficxx p.fficxx-runtime p.stdcxx ]);
          in pkgs.mkShell {
            buildInputs = [ hsenv root pkgs.nixfmt ];
            shellHook = "";
          };

        supportedCompilers = [ "ghc902" "ghc924" "ghc942" ];
      in {
        packages =
          pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler);

        devShells =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor compiler);
      });
}
