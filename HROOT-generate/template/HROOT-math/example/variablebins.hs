module Main where

import Control.Monad

import HROOT

main :: IO () 
main = do 
  tcanvas <- newTCanvas "Test" "Test" 640 480
  h1 <- newTH1D "test" "test" 100 (-5.0) 5.0 

  let bx = [-11,-5,-3,-1,1,2,3,4]

  setBins1 h1 7 bx 



  tRandom <- newTRandom 65535

  let generator = gaus tRandom 0 2

  let go n | n < 0 = return () 
           | otherwise = do 
               histfill generator h1
               go (n-1) 

  go 1000000
  draw h1 ""
  saveAs tcanvas "variablebins.pdf" ""
  delete h1
  delete tcanvas



histfill :: IO Double -> TH1D ->  IO () 
histfill gen hist = do 
  x <- gen
  fill1 hist x
  return () 
