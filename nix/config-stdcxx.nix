{ fficxxSrc, stdenv, fetchgit, packages }:

import (fficxxSrc + "/stdcxx-gen/default.nix") {
  inherit stdenv;
  haskellPackages = packages;
}
