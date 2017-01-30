{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  fficxxSrc = (import ./config.nix { inherit fetchgit; }).fficxxSrc;

  config = self: super: {
    "fficxx-runtime" = self.callPackage
      ({ mkDerivation, base, stdenv, template-haskell }:
       mkDerivation {
         pname = "fficxx-runtime";
         version = "0.2.999";
         src = "${fficxxSrc}/fficxx-runtime";
         libraryHaskellDepends = [ base template-haskell ];
         description = "Runtime for fficxx-generated library";
         license = stdenv.lib.licenses.bsd3;
       }) {};
    "fficxx" = self.callPackage
      ({ mkDerivation, base, bytestring, Cabal, containers, data-default
       , directory, either, errors, filepath, hashable, haskell-src-exts
       , lens, mtl, process, pureMD5, split, stdenv, template
       , template-haskell, text, transformers, unordered-containers
       }:
       mkDerivation {
         pname = "fficxx";
         version = "0.2.999";
         src = "${fficxxSrc}/fficxx";
         libraryHaskellDepends = [
           base bytestring Cabal containers data-default directory either
           errors filepath hashable haskell-src-exts lens mtl process pureMD5
           split template template-haskell text transformers
           unordered-containers
         ];
         description = "automatic C++ binding generation";
         license = stdenv.lib.licenses.bsd3;
       }) {};
  
  };



  newHaskellPackages = pkgs.haskellPackages.override { overrides = config; }; 

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            fficxx fficxx-runtime
            cmdargs  configurator
            containers directory filepath mtl
            process split stdenv template template-haskell transformers
            unordered-containers
          ]);
  #drv = haskellPackages.callPackage f {};

in

stdenv.mkDerivation {
  name = "HROOT-env";
  buildInputs = [ hsenv root ];
}
