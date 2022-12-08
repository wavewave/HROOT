{
  description = "HROOT";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
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

        haskellOverlay = final: self: super:
          (import ./default.nix {
            pkgs = final;
            inherit (final) root;
          } self super);

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
            hsenv = withHROOT:
              (hpkgsFor compiler).ghcWithPackages (p:
                [ p.fficxx p.fficxx-runtime p.stdcxx p.dotgen ]
                ++ (pkgs.lib.optional withHROOT p.HROOT));
            shBuildInputs = withHROOT: [
              (hsenv withHROOT)
              pkgs.cabal-install
              pkgs.root
              pkgs.nixfmt
              pkgs.graphviz
              pkgs.ormolu              
            ];
            mkShell = withHROOT:
              pkgs.mkShell {
                buildInputs = shBuildInputs withHROOT;
                shellHook = if system == "aarch64-darwin" || system == "x86_64-darwin" then
                   ''export MACOSX_DEPLOYMENT_TARGET="10.16"''
                else
                  null;
              };
          in {
            env = mkShell true;
            dev = mkShell false;
          };
        supportedCompilers = [ "ghc902" "ghc924" "ghc942" ];
      in {
        packages =
          pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler);

        devShells =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor compiler);

      });
}
