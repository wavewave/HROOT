# HROOT

Haskell binding to [ROOT](https://root.cern.ch).

[ROOT](https://root.cern.ch) is a modular scientific software toolkit providing all the functionalities needed to deal with big data processing, statistical analysis, visualisation and storage. It is mainly written in C++ but integrated with other languages.

HROOT is a haskell binding to the [ROOT](https://root.cern.ch) library. A haskell script called HROOT-generate using [fficxx](https://github.com/wavewave/fficxx) generates HROOT packages. Once generated, each package can be directly installable as a cabal package. Currently, C++ interface is defined as a haskell data structure as one can see, for example, in the module [HROOT.Data.Core.Class](HROOT-generate/lib/HROOT/Data/Core/Class.hs).

## Development and testing

For dev-shell, try:

```
$ nix develop .#ghc942.dev
$ cd workspace
$ ./build.sh
```

For those who use HROOT as library directly, try
```
$ nix develop .#ghc942.env
```
and in the shell, one can see all HROOT-* (including HROOT) packages are available.
