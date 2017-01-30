module Main where

import Foreign.C.Types
import Foreign.C.String

import HROOT

main = do
  withCString "test" $ \test ->
    withCString "histadd.pdf" $ \histaddpdf ->
      withCString "histadd.jpg" $ \histaddjpg ->
        withCString "histadd.png" $ \histaddpng ->
      
          withCString "" $ \nullstr -> do 
  
            tcanvas <- newTCanvas test test 640 480

            h1 <- newTH1F test test 100 (-10.0) 10.0  
            h2 <- newTH1F test test 100 (-10.0) 10.0 

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

            draw h1 nullstr

            saveAs tcanvas histaddpdf nullstr
            saveAs tcanvas histaddjpg nullstr
            saveAs tcanvas histaddpng nullstr

            delete h1
            delete h2
            delete tcanvas



         
histfill :: IO CDouble -> TH1F -> IO () 
histfill dist hist = do 
  x <- dist 
  fill1 hist x 
  return ()  

