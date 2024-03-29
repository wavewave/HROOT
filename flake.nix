{
  description = "HROOT";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
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

        #fficxx-version = "0.7.0.1";

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            (fficxx.haskellOverlay.${system} pkgs hself hsuper //
              # temporarily commented out until the hackage is updated.
              {
                #"fficxx" = hself.callHackage "fficxx" fficxx-version { };
                #"fficxx-runtime" =
                #  hself.callHackage "fficxx-runtime" fficxx-version { };
                #"stdcxx" = hself.callHackage "stdcxx" fficxx-version { };
                #"template" = pkgs.haskell.lib.doJailbreak hsuper.template;
                "ormolu" = pkgs.haskell.lib.overrideCabal hsuper.ormolu
                  (drv: { enableSeparateBinOutput = false; });
              } // haskellOverlay pkgs hself hsuper));

        mkPackages = compiler: {
          inherit (hpkgsFor compiler)
            HROOT HROOT-core HROOT-graf HROOT-hist HROOT-io HROOT-math HROOT-net
            HROOT-tree HROOT-RooFit HROOT-RooFit-RooStats;
        };

        mkShellFor = compiler:
          let
            hsenv = withHROOT:
              (hpkgsFor compiler).ghcWithPackages (p:
                [
                  p.fficxx
                  p.fficxx-runtime
                  p.stdcxx
                  p.optparse-applicative
                  p.dotgen
                ] ++ (pkgs.lib.optional withHROOT p.HROOT));
            pyenv = pkgs.python3.withPackages
              (p: [ p.sphinx p.sphinx_rtd_theme p.myst-parser ]);
            shBuildInputs = withHROOT: [
              (hsenv withHROOT)
              pyenv
              pkgs.cabal-install
              pkgs.root
              pkgs.nixfmt
              pkgs.graphviz
              # this is due to https://github.com/NixOS/nixpkgs/issues/140774
              (hpkgsFor "ghc924").ormolu
            ];
            mkShell = withHROOT:
              pkgs.mkShell {
                buildInputs = shBuildInputs withHROOT;
                shellHook = ''
                  export PS1="\n[HROOT:\w]$ \0"
                '' + (if system == "aarch64-darwin" || system
                == "x86_64-darwin" then
                  ''export MACOSX_DEPLOYMENT_TARGET="10.16"''
                else
                  null);
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
