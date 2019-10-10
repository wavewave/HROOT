{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, threadDelay )
import Control.Monad         ( forever )
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
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
  nref <- newIORef (0 :: Int)
  alloca $ \pargc -> do
    alloca $ \pargv -> do
      poke pargc (0::CInt)
      poke pargv (""::CString)
      gsys <- gSystem
      tapp <- newTApplication ("test"::CString) pargc pargv
      tcanvas <- newTCanvas ("Test"::CString) ("Test"::CString) 640 480
      h2 <- newTH2F ("test"::CString) ("test"::CString) 100 (-5.0) 5.0 100 (-5.0) 5.0
      tRandom <- newTRandom 65535

      let dist1 = gaus tRandom 0 2
          dist2 = gaus tRandom 0 2

      let go n | n <= 0 = return ()
               | otherwise = do
                   histfill dist1 dist2 h2
                   go (n-1)

      -- run tapp 1
      draw h2 ("lego"::CString)

      forkIO $ forever $ do
        threadDelay 1000000
        n <- readIORef nref
        print n
        go 10 -- 10000000
        update tcanvas
        paint tcanvas (""::CString)
        modifyIORef' nref (+10)

      forever $ do
        threadDelay (1000000 `div` 60) -- every 1/60 sec
        processEvents gsys
      delete h2
      delete tapp



histfill :: IO CDouble -> IO CDouble-> TH2F ->  IO ()
histfill dist1 dist2 hist = do
  x <- dist1
  y <- dist2
  fill2 hist x y
  return ()
