{ root }:

self: super: {
  "HROOT-core" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, stdenv
    , template-haskell
    }:
    mkDerivation {
      pname = "HROOT-core";
      version = "0.9.0.1";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime template-haskell
      ];
      setupHaskellDepends = [ root ];
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to ROOT Core modules";
      license = stdenv.lib.licenses.lgpl21;
    }) {};
    
  "HROOT-io" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, HROOT-core, stdenv
    , template-haskell
    }:
    mkDerivation {
      pname = "HROOT-io";
      version = "0.9.0.1";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime HROOT-core template-haskell
      ];
      setupHaskellDepends = [ root ];
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to ROOT IO modules";
      license = stdenv.lib.licenses.lgpl21;
    }) {};

  "HROOT-math" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, HROOT-core, stdenv
    , template-haskell
    }:
    mkDerivation {
      pname = "HROOT-math";
      version = "0.9.0.1";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime HROOT-core template-haskell
      ];
      setupHaskellDepends = [ root ];
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to ROOT Math modules";
      license = stdenv.lib.licenses.lgpl21;
    }) {};

  "HROOT-tree" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, HROOT-core, stdenv
    , template-haskell
    }:
    mkDerivation {
      pname = "HROOT-tree";
      version = "0.9.0.1";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime HROOT-core template-haskell
      ];
      setupHaskellDepends = [ root ];      
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to ROOT Tree modules";
      license = stdenv.lib.licenses.lgpl21;
    }) {};

  "HROOT-hist" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, HROOT-core, stdenv
    , template-haskell
    }:
    mkDerivation {
      pname = "HROOT-hist";
      version = "0.9.0.1";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime HROOT-core template-haskell
      ];
      setupHaskellDepends = [ root ];      
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to ROOT Hist modules";
      license = stdenv.lib.licenses.lgpl21;
    }) {};

  "HROOT-graf" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, HROOT-core
    , HROOT-hist, stdenv, template-haskell
    }:
    mkDerivation {
      pname = "HROOT-graf";
      version = "0.9.0.1";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime HROOT-core HROOT-hist template-haskell
      ];
      setupHaskellDepends = [ root ];      
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to ROOT Graf modules";
      license = stdenv.lib.licenses.lgpl21;
    }) {};

  "HROOT" = self.callPackage
    ({ mkDerivation, base, fficxx, fficxx-runtime, HROOT-core
    , HROOT-graf, HROOT-hist, HROOT-io, HROOT-math, HROOT-tree, stdenv
    , template-haskell
    }:
    mkDerivation {
      pname = "HROOT";
      version = "0.9.0.1";
      libraryHaskellDepends = [
        base fficxx fficxx-runtime HROOT-core HROOT-graf HROOT-hist
        HROOT-io HROOT-math HROOT-tree template-haskell
      ];
      setupHaskellDepends = [ root ];      
      homepage = "http://ianwookim.org/HROOT";
      description = "Haskell binding to the ROOT data analysis framework";
      license = stdenv.lib.licenses.lgpl21;
    }) {};



}