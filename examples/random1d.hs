module Main where

import Control.Monad
import Foreign.C.Types (CDouble)
import Foreign.C.String (newCString)
--
-- import HROOT
import HROOT.Core
import HROOT.Hist
import HROOT.Graf
import HROOT.Math


main :: IO ()
main = do
  cTest <- newCString "Test"
  ctest <- newCString "test"
  cpdf  <- newCString "random1d.pdf"
  cjpg  <- newCString "random1d.jpg"
  cpng  <- newCString "random1d.png"
  cnull <- newCString ""

  tcanvas <- newTCanvas cTest cTest 640 480
  h1 <- newTH1F ctest ctest 100 (-5.0) 5.0

  tRandom <- newTRandom 65535

  let generator = gaus tRandom 0 2

  let go n | n < 0 = return ()
           | otherwise = do
               histfill generator h1
               go (n-1)

  go 1000000
  draw h1 cnull
  saveAs tcanvas cpdf cnull
  saveAs tcanvas cjpg cnull
  saveAs tcanvas cpng cnull
  delete h1
  delete tcanvas



histfill :: IO CDouble -> TH1F ->  IO ()
histfill gen hist = do
  x <- gen
  fill1 hist x
  return ()
