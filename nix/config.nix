{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "2ddd32a40710558e5ebbd1f3bba0d5951c1dbe8a";
    sha256 = "15227grpdbpds9dzlnr78l78nx2d1v6kgn0iblnilsmpjk80bxr9";
  };

in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}
