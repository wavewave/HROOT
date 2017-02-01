{ fetchgit }:

let 
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "fcf0a9851a1a803195765d80ae34cb320355fd55";
    sha256 = "0c1yy0zrklvnlgf58ka2156q9yg3cs4iwdf2iigdprlsmd8mdc4h";
  };
in 

self: super: {
  "fficxx-runtime" = self.callPackage (import (fficxxSrc + "/fficxx-runtime")) {};
  "fficxx" = self.callPackage (import (fficxxSrc + "/fficxx")) {}; 

  "HROOT-generate" = self.callPackage (import ./default.nix) {};
}



