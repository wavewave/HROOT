{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "c9f31438b7283e90209384ed734c339e1019c5bc";
    sha256 = "170qi5s64lh2fdwnysar7x128jqivbajriy25szjf3yqbss13nin";
  };
in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}
