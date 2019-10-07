{ fetchgit }:

let 
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "2366462d01131a6ef232e2a5cd5784e83c7046ae";
    sha256 = "08g4padwbxsk95rblkhzr0pjm5ry9a7nksncqzk5c3j2g7zd70ik";
  };
in 

self: super: {
  "fficxx-runtime" = self.callPackage (import (fficxxSrc + "/fficxx-runtime")) {};
  "fficxx" = self.callPackage (import (fficxxSrc + "/fficxx")) {}; 

  "HROOT-generate" = self.callPackage (import ./default.nix) {};
}



