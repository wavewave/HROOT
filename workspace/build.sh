rm -rf dist-newstyle
cabal new-run -- HROOT-generate ../HROOT-generate/template
# cd ../HROOT-generate; cabal new-build ; cd ../workspace
#../HROOT-generate/dist-newstyle/build/x86_64-linux/ghc-8.6.5/HROOT-generate-0.10.0.1/x/HROOT-generate/build/HROOT-generate/HROOT-generate ../HROOT-generate/template
cabal new-build HROOT
cabal new-exec -- ghc random2dApp.hs
cabal new-exec -- ghc graph2d.hs
cabal new-exec -- ghc httpserver.hs
cabal new-exec -- ghc datetime.hs

