{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "398a00fc3c2d474b61e729d65e817eefb7e5bfa4";
    sha256 = "1lylcxnm07njzzfzmg9iq74qpbmdl0vg5pi2mmrakjgf9chxxb3z";
  };
in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}
