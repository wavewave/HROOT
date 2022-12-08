{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Foreign.C.Types
--
import HROOT
import System.IO.Unsafe

main :: IO ()
main = do
  tcanvas <- newTCanvas ("Test" :: ByteString) ("Test" :: ByteString) 640 480
  h2 <- newTH2F ("test" :: ByteString) ("test" :: ByteString) 100 (-5.0) 5.0 100 (-5.0) 5.0

  tRandom <- newTRandom 65535

  let generator = gaus tRandom 0 2

  let go n
        | n < 0 = return ()
        | otherwise = do
          histfill generator generator h2
          go (n - 1)

  go 1000000
  draw h2 ("lego" :: ByteString)
  saveAs tcanvas ("random2d.pdf" :: ByteString) ("" :: ByteString)
  delete h2
  delete tcanvas

histfill :: IO CDouble -> IO CDouble -> TH2F -> IO ()
histfill dist1 dist2 hist = do
  x <- dist1
  y <- dist2
  fill2 hist x y
  return ()
