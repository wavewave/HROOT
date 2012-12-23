module Main where

import HROOT
{-
import HROOT.Class.Deletable
import HROOT.Class.TH1
import HROOT.Class.TH1F
import HROOT.Class.TCanvas
import HROOT.Class.TRandom 
import HROOT.Class.TObject 
-}

main = do 
  tcanvas <- newTCanvas "test" "test" 640 480
  
  h1 <- newTH1F "test" "test" 100 (-10.0) 10.0  
  h2 <- newTH1F "test" "test" 100 (-10.0) 10.0 
  
  tRandom <- newTRandom 65535

  let dist1 = gaus tRandom 0 2 
      dist2 = gaus tRandom 3 1 

  let go n | n < 0 = return () 
           | otherwise = do 
               histfill dist1 h1 
               histfill dist2 h2 
               go (n-1)
  go 1000000 
 
  add h1 h2 1.0 
   
  draw h1 ""   

  saveAs tcanvas "histadd.pdf" "" 
  saveAs tcanvas "histadd.jpg" ""
  saveAs tcanvas "histadd.png" ""

  delete h1
  delete h2
  delete tcanvas

  return () 

         
histfill :: IO Double -> TH1F -> IO () 
histfill dist hist = do 
  x <- dist 
  fill1 hist x 
  return ()  

