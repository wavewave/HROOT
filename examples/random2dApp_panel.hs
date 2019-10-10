{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, threadDelay )
import Control.Monad         ( forever, when )
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
      --                                                   xL   yB   xR   yT
      tpad1 <- newTPad ("pad1"::CString) ("pad1"::CString) 0.05 0.05 0.49 0.49
      tpad2 <- newTPad ("pad2"::CString) ("pad2"::CString) 0.05 0.51 0.49 0.95
      tpad3 <- newTPad ("pad3"::CString) ("pad3"::CString) 0.51 0.05 0.95 0.49
      tpad4 <- newTPad ("pad4"::CString) ("pad4"::CString) 0.51 0.51 0.95 0.95

      cd tcanvas 0      
      draw tpad1 (""::CString)
      cd tcanvas 0      
      draw tpad2 (""::CString)
      cd tcanvas 0      
      draw tpad3 (""::CString)
      cd tcanvas 0      
      draw tpad4 (""::CString)      

      cd tpad2 0
      h2 <- newTH2F ("test"::CString) ("test"::CString) 100 (-5.0) 5.0 100 (-5.0) 5.0
      draw h2 ("lego"::CString)


      cd tpad1 0
      h1x <- tH2_ProjectionX (upcastTH2 h2) ("tdk"::CString) 0 (-1) (""::CString) 
      draw h1x (""::CString) 


      tRandom <- newTRandom 65535

      let dist1 = gaus tRandom 0 2
          dist2 = gaus tRandom 0 2

      let go n | n <= 0 = return ()
               | otherwise = do
                   histfill dist1 dist2 h2
                   go (n-1)


      

      forkIO $ forever $ do
        threadDelay 100000
        n <- readIORef nref
        when (n `mod` 10000 == 0) $
          print n
        go 1000
        
        modifyIORef' nref (+10000)

      forkIO $ forever $ do
        threadDelay (1000000 `div` 60) -- every 1/60 sec
        update tcanvas
        paint tcanvas (""::CString)


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
