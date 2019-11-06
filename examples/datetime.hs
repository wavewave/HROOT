{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Data.String             ( IsString(fromString) )
import Foreign.C.String        ( CString, newCString )
import System.IO.Unsafe        ( unsafePerformIO )
--
import STD.Deletable.Interface ( delete )
import HROOT



instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

main = do
  tcanvas <- newTCanvas ("test"::CString) ("test"::CString) 640 480

  t0 <- newTDatime 2002 1 1 0 0 0
  x0 <- convert t0 0
  t1 <- newTDatime 2002 09 23 00 00 00
  x1 <- (-) <$> convert t1 0 <*> return x0
  t2 <- newTDatime 2003 03 07 00 00 00
  x2 <- (-) <$> convert t2 0 <*> return x0

  putStrLn $ show x0
  putStrLn $ show x1
  putStrLn $ show x2

  h1 <- newTH1F ("test"::CString) ("test"::CString) 100 (fromIntegral x1) (fromIntegral x2)

  xaxis <- tH1_GetXaxis (upcastTH1 h1)
  setTimeOffset xaxis (fromIntegral x0) ("local"::CString)
  setTimeDisplay xaxis 1
  setTimeFormat xaxis ("%Y/%m/%d"::CString)

  draw h1 (""::CString)

  saveAs tcanvas ("datetime.pdf"::CString) (""::CString)

  delete h1
  delete tcanvas

  return ()
