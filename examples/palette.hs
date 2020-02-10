{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 ( ByteString )
import Data.Foldable         ( for_ )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.C.String      ( newCString )
import Foreign.C.Types       ( CInt )
import Foreign.Storable      ( poke )
--
import HROOT

main :: IO ()
main = do
  alloca $ \pargc -> do
    alloca $ \pargv -> do
      poke pargc (0::CInt)
      poke pargv =<< newCString ""
      tapp <- newTApplication ("test"::ByteString) pargc pargv
      tcanvas <- newTCanvas ("Test"::ByteString) ("Test"::ByteString) 1024 768
      arr <- tColor_GetPalette
      n <- tArray_GetSize (upcastTArray arr)
      for_ [0..n-1] $ \i -> do
        v <- getAt arr i
        print v
      run tapp 1

