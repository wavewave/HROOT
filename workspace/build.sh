
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

cabal build fficxx
cabal exec runhaskell ../../fficxx/stdcxx-gen/Gen.hs
cabal build stdcxx
cabal build HROOT-generate
cabal run -- HROOT-generate gen -t ../HROOT-generate/template
#cabal build HROOT-core
cabal build HROOT

