{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, forkOn, threadDelay )
-- import Control.Concurrent.MVar ( newMVar, putMVar, takeMVar )
import Control.Monad         ( forever, replicateM_, void, when )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
import Foreign.C.Types       ( CDouble, CInt )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( fromBool )
import Foreign.Storable      ( poke )
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
        -- mutex <- newMVar ()
        mutex <- newTMutex (fromBool False)

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

        draw h2 ("lego"::ByteString)


        forkIO $ replicateM_ 100000000 $ do
          n <- readIORef nref
          go 1
          modifyIORef' nref (+1)

        forkIO $ forever $ do
          threadDelay (1000000 `div` 60) -- every 1/60 sec
          lock mutex   -- takeMVar mutex
          processEvents gsys
          unLock mutex -- putMVar mutex ()

        forkIO $ forever $ do
          threadDelay (1000000 `div` 60) -- every 1/60 sec
          lock mutex   -- takeMVar mutex
          update tcanvas
          paint tcanvas (""::ByteString)
          unLock mutex -- putMVar mutex ()

        -- idling
        forever $ do
          threadDelay 10000000

        delete h2
        delete tapp


histfill :: IO CDouble -> IO CDouble-> TH2F ->  IO ()
histfill dist1 dist2 hist = do
  x <- dist1
  y <- dist2
  fill2 hist x y
  return ()
