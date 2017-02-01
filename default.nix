{ mkDerivation, base, Cabal, cmdargs, configurator
, containers, directory, fficxx, filepath, HStringTemplate, mtl
, process, split, stdenv, template-haskell, transformers
, unordered-containers
}:

mkDerivation {
  pname = "HROOT-generate";
  version = "0.9";
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
}