{ root }:

self: super: {
  "HROOT-core" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, stdenv
    , template-haskell
    }:
    mkDerivation {
      pname = "HROOT-core";
      version = "0.9";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime template-haskell
      ];
      setupHaskellDepends = [
        root
      ];
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to ROOT Core modules";
      license = stdenv.lib.licenses.lgpl21;
    }) {};
}