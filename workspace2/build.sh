
# delete only generated
rm -rf stdcxx
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

cabal v2-build fficxx
cabal v2-exec runhaskell ../../fficxx/stdcxx-gen/Gen.hs
cabal v2-build stdcxx
cabal v2-build HROOT-generate
cabal v2-run -- HROOT-generate ../HROOT-generate/template
cabal v2-build HROOT

