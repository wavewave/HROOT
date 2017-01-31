{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.ByteString.Char8 (ByteString,useAsCString)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable

import HROOT

main :: IO () 
main = do
  ys <- mapM (\x->useAsCString x return) ["test"]
  withArray ys $ \pargv -> do
    alloca $ \pargc -> do
      poke pargc 0      

      tapp <- newTApplication ("test" :: ByteString) pargc pargv -- ["test" :: ByteString]
      tcanvas <- newTCanvas ("Test" :: ByteString) ("Test" :: ByteString) 640 480
      h2 <- newTH2F ("test" :: ByteString) ("test" :: ByteString) 100 (-5.0) 5.0 100 (-5.0) 5.0 
      tRandom <- newTRandom 65535

      let dist1 = gaus tRandom 0 2 
          dist2 = gaus tRandom 0 2 

      let go n | n < 0 = return () 
               | otherwise = do 
                   histfill dist1 dist2 h2
                   go (n-1) 

      go 1000000
      draw h2 ("lego" :: ByteString)
      run tapp 1 
      delete h2
      delete tapp



histfill :: IO CDouble -> IO CDouble-> TH2F ->  IO () 
histfill dist1 dist2 hist = do 
  x <- dist1
  y <- dist2
  fill2 hist x y 
  return () 
