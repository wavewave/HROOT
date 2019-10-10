{ fetchgit }:

let
  fficxxSrc = fetchgit {
    url = "git://github.com/wavewave/fficxx.git";
    rev = "fa39e291592aae14d24a7d9ce39f9efcb65dc4ef";
    sha256 = "07q7hkgxpnxjpz4j69592dpmkh8xj0d95xgvsqwm0ww49yfl3k8p";
  };

in

self: super: {
  "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
  "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
}
