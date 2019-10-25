{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, threadDelay )
import Control.Monad         ( forever, replicateM_, when )
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
import Data.String           ( IsString(fromString) )
import Foreign.C.Types       ( CBool, CDouble, CInt )
import Foreign.C.String      ( CString, newCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( fromBool )
import Foreign.Storable      ( poke )
import System.IO.Unsafe      ( unsafePerformIO )
--
import HROOT


kUseGlobal = 0
kUseCompiledDefault = 404
kUseAnalysis = 404
kUseGeneralPurpose = 101
kUseSmallest = 207

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

histfill :: IO CDouble -> IO CDouble-> TH2F ->  IO ()
histfill dist1 dist2 hist = do
  x <- dist1
  y <- dist2
  fill2 hist x y
  return ()

main :: IO ()
main = do
  putStrLn "httpserver"

  nref <- newIORef (0::Int)

  gsys <- gSystem
  -- tapp <- newTApplication ("test"::CString) pargc pargv
  -- tcanvas <- newTCanvas ("Test"::CString) ("Test"::CString) 640 480
  -- toggleEditor tcanvas
  -- toggleEventStatus tcanvas
  -- toggleToolBar tcanvas
  -- toggleToolTips tcanvas
  mfile <- newTMemFile ("job1.root"::CString) ("RECREATE"::CString) ("Demo ROOT file with histogram"::CString) kUseGeneralPurpose

  h2 <- newTH2F ("test"::CString) ("test"::CString) 100 (-5.0) 5.0 100 (-5.0) 5.0
  write_ mfile
  tRandom <- newTRandom 65535

  let dist1 = gaus tRandom 0 2
      dist2 = gaus tRandom 0 2

  let go n | n <= 0 = return ()
           | otherwise = do
               histfill dist1 dist2 h2
               go (n-1)

  serv <- newTHttpServer ("http:8080?top=job1.root"::CString)
  tHttpServer_SetReadOnly serv (fromBool True::CBool)

  forkIO $ replicateM_ 1000000 $ do
    n <- readIORef nref
    when (n `mod` 100 == 0) $
      print n
    threadDelay 1000
    go 1

    modifyIORef' nref (+1)

  forkIO $ forever $ do
    threadDelay (1000000 `div` 10) -- every 1/10 sec
    write_ mfile
    -- update tcanvas
    -- paint tcanvas (""::CString)


  forever $ do
    threadDelay (1000000 `div` 60) -- every 1/60 sec
    processEvents gsys

  delete h2
  delete mfile
  -- delete tapp
