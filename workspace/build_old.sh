rm -rf dist-newstyle
rm -rf HROOT
rm -rf HROOT-core
rm -rf HROOT-graf
rm -rf HROOT-hist
rm -rf HROOT-io
rm -rf HROOT-math
rm -rf HROOT-net
rm -rf HROOT-RooFit
rm -rf HROOT-RooFit-RooStats
rm -rf HROOT-tree
cabal new-build HROOT-generate
cabal new-run -- HROOT-generate ../HROOT-generate/template
cabal new-build HROOT
cabal new-exec -- ghc random1d.hs
cabal new-exec -- ghc random2dApp.hs
cabal new-exec -- ghc graph2d.hs
cabal new-exec -- ghc httpserver.hs
cabal new-exec -- ghc datetime.hs
cabal new-exec -- ghc -threaded random2dApp_multithread.hs
