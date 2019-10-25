{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, threadDelay )
import Control.Monad         ( forever, replicateM_, void, when )
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
import Data.String           ( IsString(fromString) )
import Foreign.C.Types       ( CBool, CDouble, CInt )
import Foreign.C.String      ( CString, newCString )
import Foreign.Marshal.Utils ( fromBool )
import System.IO.Unsafe      ( unsafePerformIO )
--
import HROOT

kUseGlobal :: CInt
kUseGlobal = 0

kUseCompiledDefault :: CInt
kUseCompiledDefault = 404

kUseAnalysis :: CInt
kUseAnalysis = 404

kUseGeneralPurpose :: CInt
kUseGeneralPurpose = 101

kUseSmallest :: CInt
kUseSmallest = 207

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

histfill :: IO CDouble -> IO CDouble-> TH2F ->  IO ()
histfill dist1 dist2 hist = do
  x <- dist1
  y <- dist2
  void $ fill2 hist x y

main :: IO ()
main = do
  putStrLn "httpserver"

  nref <- newIORef (0::Int)

  gsys <- gSystem

  mfile <- newTMemFile ("job1.root"::CString) ("RECREATE"::CString) ("Demo ROOT file with histogram"::CString) kUseGeneralPurpose

  h2 <- newTH2F ("test"::CString) ("test"::CString) 100 (-5.0) 5.0 100 (-5.0) 5.0
  void $ write_ mfile
  tRandom <- newTRandom 65535

  let dist1 = gaus tRandom 0 2
      dist2 = gaus tRandom 0 2

  let go :: Int -> IO ()
      go n | n <= 0 = return ()
           | otherwise = do
               histfill dist1 dist2 h2
               go (n-1)

  serv <- newTHttpServer ("http:8080?top=job1.root"::CString)
  tHttpServer_SetReadOnly serv (fromBool True::CBool)

  void $ forkIO $ replicateM_ 1000000 $ do
    n <- readIORef nref
    when (n `mod` 100 == 0) $
      print n
    threadDelay 1000
    go 1

    modifyIORef' nref (+1)

  void $ forkIO $ forever $ do
    threadDelay (1000000 `div` 10) -- every 1/10 sec
    write_ mfile

  void $ forever $ do
    threadDelay (1000000 `div` 60) -- every 1/60 sec
    processEvents gsys

  delete h2
  delete mfile
