{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import HROOT

main = do
  tcanvas <- newTCanvas ("test" :: ByteString) ("test" :: ByteString) 640 480
  h1 <- newTH1F ("test" :: ByteString) ("test" :: ByteString) 100 1 10
  h2 <- newTH1F ("test" :: ByteString) ("test" :: ByteString) 100 1 10

  fill1 h1 5
  fill1 h2 6

  add h1 h2 1.0

  draw h1 ("" :: ByteString)

  tfile <- newTFile ("test.root" :: ByteString) ("NEW" :: ByteString) ("" :: ByteString) 1
  write h1 ("" :: ByteString) 0 0
  close tfile ("" :: ByteString)

  delete tfile
  delete h1
  delete h2

  delete tcanvas
