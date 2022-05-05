# HROOT

Haskell binding to [ROOT](https://root.cern.ch).

[ROOT](https://root.cern.ch) is a modular scientific software toolkit providing all the functionalities needed to deal with big data processing, statistical analysis, visualisation and storage. It is mainly written in C++ but integrated with other languages.

HROOT is a haskell binding to the [ROOT](https://root.cern.ch) library. A haskell script called HROOT-generate using [fficxx](https://github.com/wavewave/fficxx) generates HROOT packages. Once generated, each package can be directly installable as a cabal package. Currently, C++ interface is defined as a haskell data structure as one can see, for example, in the module [HROOT.Data.Core.Class](HROOT-generate/lib/HROOT/Data/Core/Class.hs).

## fficxx-projects

fficxx-generated Haskell binding projects are collected in a meta-repo
[fficxx-projects](https://github.com/wavewave/fficxx-projects). It is convenient for users
to check out the repo and test HROOT in that environment as described there.

## Development and testing

However, if one wants to test this only in this repo, try:

```
$ nix develop .#
$ cd workspace
$ ./build.sh
```
