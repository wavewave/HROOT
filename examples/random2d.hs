{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.String           ( IsString(fromString) )
import Foreign.C.Types       ( CDouble, CInt )
import Foreign.C.String      ( CString, newCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable      ( poke )
import System.IO.Unsafe      ( unsafePerformIO )
--
import HROOT

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

main :: IO ()
main = do
  tcanvas <- newTCanvas ("Test"::CString) ("Test"::CString) 640 480
  h2 <- newTH2F ("test"::CString) ("test"::CString) 100 (-5.0) 5.0 100 (-5.0) 5.0

  tRandom <- newTRandom 65535

  let generator = gaus tRandom 0 2

  let go n | n < 0 = return ()
           | otherwise = do
               histfill generator generator h2
               go (n-1)

  go 1000000
  draw h2 ("lego"::CString)
  saveAs tcanvas ("random2d.pdf"::CString) (""::CString)
  saveAs tcanvas ("random2d.jpg"::CString) (""::CString)
  saveAs tcanvas ("random2d.png"::CString) (""::CString)
  delete h2
  delete tcanvas



histfill :: IO CDouble -> IO CDouble-> TH2F ->  IO ()
histfill dist1 dist2 hist = do
  x <- dist1
  y <- dist2
  fill2 hist x y
  return ()
