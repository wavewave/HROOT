{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  config = self: super: {
    fficxx = haskell.lib.doJailbreak super.fficxx;
    "HROOT-generate" = self.callPackage
      ({ mkDerivation, base, Cabal, cmdargs, configurator
      , containers, directory, fficxx, filepath, HStringTemplate, mtl
      , process, split, stdenv, template-haskell, transformers
      , unordered-containers
      }:
      mkDerivation {
        pname = "HROOT-generate";
        version = "0.8.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base Cabal cmdargs containers directory fficxx filepath
          HStringTemplate mtl process split template-haskell transformers
          unordered-containers
        ];
        executableHaskellDepends = [
          base Cabal cmdargs configurator containers directory fficxx
          filepath HStringTemplate mtl process split template-haskell
          transformers unordered-containers
        ];
        description = "automatic HROOT binding generation";
        license = stdenv.lib.licenses.lgpl21;
      }) {};
  };



  newHaskellPackages = pkgs.haskellPackages.override { overrides = config; }; 

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            HROOT-generate fficxx fficxx-runtime
          ]);
  #drv = haskellPackages.callPackage f {};

in

stdenv.mkDerivation {
  name = "HROOT-env";
  buildInputs = [ hsenv root ];
}
