{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
--
import HROOT

main = do
  tcanvas <- newTCanvas ("test" :: ByteString) ("test" :: ByteString) 640 480

  t0 <- newTDatime 2002 1 1 0 0 0
  x0 <- convert t0 0
  t1 <- newTDatime 2002 09 23 00 00 00
  x1 <- (-) <$> convert t1 0 <*> return x0
  t2 <- newTDatime 2003 03 07 00 00 00
  x2 <- (-) <$> convert t2 0 <*> return x0

  putStrLn $ show x0
  putStrLn $ show x1
  putStrLn $ show x2

  h1 <- newTH1F ("test" :: ByteString) ("test" :: ByteString) 100 (fromIntegral x1) (fromIntegral x2)

  xaxis <- tH1GetXaxis (upcastTH1 h1)
  setTimeOffset xaxis (fromIntegral x0) ("local" :: ByteString)
  setTimeDisplay xaxis 1
  setTimeFormat xaxis ("%Y/%m/%d" :: ByteString)

  draw h1 ("" :: ByteString)

  saveAs tcanvas ("datetime.pdf" :: ByteString) ("" :: ByteString)

  delete h1
  delete tcanvas

  return ()
