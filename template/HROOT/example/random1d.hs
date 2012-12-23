module Main where

import Control.Monad

import HROOT

main :: IO () 
main = do 
  tcanvas <- newTCanvas "Test" "Test" 640 480
  h1 <- newTH1F "test" "test" 100 (-5.0) 5.0 

  tRandom <- newTRandom 65535

  let generator = gaus tRandom 0 2

  let go n | n < 0 = return () 
           | otherwise = do 
               histfill generator h1
               go (n-1) 

  go 1000000
  draw h1 ""
  saveAs tcanvas "random1d.pdf" ""
  saveAs tcanvas "random1d.jpg" "" 
  saveAs tcanvas "random1d.png" "" 
  delete h1
  delete tcanvas



histfill :: IO Double -> TH1F ->  IO () 
histfill gen hist = do 
  x <- gen
  fill1 hist x
  return () 
