{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, threadDelay )
import Control.Monad         ( forever, replicateM_, when )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
import Data.String           ( IsString(fromString) )
import Foreign.C.Types       ( CDouble, CInt )
import Foreign.C.String      ( CString, newCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable      ( poke )
import System.IO.Unsafe      ( unsafePerformIO )
--
import HROOT
import STD.Deletable.Interface (delete)


main :: IO ()
main = do
  nref <- newIORef (0 :: Int)
  alloca $ \pargc -> do
    alloca $ \pargv -> do
      B.useAsCString "" $ \cs -> do
        poke pargc (0::CInt)
        poke pargv cs
        gsys <- gSystem
        tapp <- newTApplication ("test"::ByteString) pargc pargv
        tcanvas <- newTCanvas ("Test"::ByteString) ("Test"::ByteString) 640 480
        toggleEditor tcanvas
        toggleEventStatus tcanvas
        toggleToolBar tcanvas
        toggleToolTips tcanvas
        h2 <- newTH2F ("test"::ByteString) ("test"::ByteString) 100 (-5.0) 5.0 100 (-5.0) 5.0
        tRandom <- newTRandom 65535

        let dist1 = gaus tRandom 0 2
            dist2 = gaus tRandom 0 2

        let go n | n <= 0 = return ()
                 | otherwise = do
                     histfill dist1 dist2 h2
                     go (n-1)

        -- run tapp 1
        draw h2 ("lego"::ByteString)

        forkIO $ replicateM_ 1000000 $ do
          n <- readIORef nref
          when (n `mod` 100 == 0) $
            print n
          go 1
          modifyIORef' nref (+1)

        forkIO $ forever $ do
          threadDelay (1000000 `div` 60) -- every 1/60 sec
          update tcanvas
          paint tcanvas (""::ByteString)

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
