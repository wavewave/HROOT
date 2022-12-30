.. fficxx documentation master file, created by
   sphinx-quickstart on Thu Dec 29 11:23:22 2022.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to HROOT's documentation
================================

Haskell binding to `ROOT <https://root.cern.ch>`_

`ROOT <https://root.cern.ch>`_ is a modular scientific software toolkit providing all the functionalities needed to deal with big data processing, statistical analysis, visualisation and storage. It is mainly written in C++ but integrated with other languages.

HROOT is a haskell binding to the `ROOT <https://root.cern.ch>`_ library. A haskell script called HROOT-generate using `fficxx <https://wavewave.github.io/wavewave/fficxx>`_ generates HROOT packages. Once generated, each package can be directly installable as a cabal package. Currently, C++ interface is defined as a haskell data structure as one can see, for example, in the module HROOT.Data.Core.Class.

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

.. toctree::
   :maxdepth: 2
   :caption: Contents:

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
