{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Foreign.C.Types
--
import HROOT

main :: IO ()
main = do
  tcanvas <- newTCanvas ("Test" :: ByteString) ("Test" :: ByteString) 640 480
  h2 <- newTH2F ("test" :: ByteString) ("test" :: ByteString) 100 (-5.0) 5.0 100 (-5.0) 5.0
  tRandom <- newTRandom 65535
  let generator1 = gaus tRandom 0 2
      generator2 = gaus tRandom 1 0.5
  let go n
        | n <= 0 = return ()
        | otherwise = do
          histfill generator1 generator1 h2
          histfill generator2 generator2 h2
          go (n - 1)
  go 1000000

  h1 <- tH2ProjectionX (upcastTH2 h2) ("tdk" :: ByteString) 0 (-1) ("" :: ByteString)

  draw h1 ("" :: ByteString)
  saveAs tcanvas ("projection.pdf" :: ByteString) ("" :: ByteString)
  delete h1
  delete h2
  delete tcanvas

histfill :: IO CDouble -> IO CDouble -> TH2F -> IO ()
histfill dist1 dist2 hist = do
  x <- dist1
  y <- dist2
  fill2 hist x y
  return ()
