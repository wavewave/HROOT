{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.String      ( IsString(fromString) )
import Foreign.C.Types  ( CDouble )
import Foreign.C.String ( CString, newCString )
import System.IO.Unsafe ( unsafePerformIO )
--
import HROOT

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

main :: IO ()
main = do
  tcanvas <- newTCanvas ("Test"::CString) ("Test"::CString) 640 480
  h1 <- newTH1F ("test"::CString) ("test"::CString) 100 (-5.0) 5.0

  tRandom <- newTRandom 65535

  let generator = gaus tRandom 0 2

  let go n | n < 0 = return ()
           | otherwise = do
               histfill generator h1
               go (n-1)

  go 1000000
  draw h1 (""::CString)
  saveAs tcanvas ("random1d.pdf"::CString) (""::CString)
  saveAs tcanvas ("random1d.jpg"::CString) (""::CString)
  saveAs tcanvas ("random1d.png"::CString) (""::CString)
  delete h1
  delete tcanvas



histfill :: IO CDouble -> TH1F ->  IO ()
histfill gen hist = do
  x <- gen
  fill1 hist x
  return ()
