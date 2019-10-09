{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "5b980537ca483329249985476a0aa6549bac3294";
    sha256 = "0dlnyj23d747jb9ahzrx8s0fbd48rkw9h1w2i06aiiw9vkpam0lz";
  };

in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}
