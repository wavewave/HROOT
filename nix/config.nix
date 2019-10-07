{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "8168f6448d9364da6863346ca8dbd5cd56280de6";
    sha256 = "1zd1j294yimh7hlaiisylb9iqss26aac1wr30dg3yb8g5pz019by";
  };
in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}
