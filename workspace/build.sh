rm -rf dist-newstyle
cabal new-run -- HROOT-generate ../HROOT-generate/template
cabal new-build HROOT
cabal new-exec -- ghc random1d.hs
cabal new-exec -- ghc random2dApp.hs
cabal new-exec -- ghc graph2d.hs
cabal new-exec -- ghc httpserver.hs
cabal new-exec -- ghc datetime.hs

