{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "ac829cd8f315a4a7c1db13db9595f40dda0df7c8";
    sha256 = "1nz3hjx8jqsb200mcg7jm88p1004ka4h11wipzwcay87lxrsa4f1";
  };

in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}
