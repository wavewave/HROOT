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
  h1 <- newTH1D ("test" :: ByteString) ("test" :: ByteString) 100 (-5.0) 5.0

  let bx = [-11, -5, -3, -1, 1, 2, 3, 4]

  setBins1 h1 7 bx

  tRandom <- newTRandom 65535

  let generator = gaus tRandom 0 2

  let go n
        | n < 0 = return ()
        | otherwise = do
          histfill generator h1
          go (n - 1)

  go 1000000
  draw h1 ("" :: ByteString)
  saveAs tcanvas ("variablebins.pdf" :: ByteString) ("" :: ByteString)
  delete h1
  delete tcanvas

histfill :: IO CDouble -> TH1D -> IO ()
histfill gen hist = do
  x <- gen
  fill1 hist x
  return ()
