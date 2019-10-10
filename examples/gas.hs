{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, threadDelay )
import Control.Monad         ( forever
                             , replicateM
                             , replicateM_
                             , void
                             , when
                             )
import Control.Monad.Loops   ( iterateM_ )
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
import Data.String           ( IsString(fromString) )
import Foreign.C.Types       ( CDouble, CInt )
import Foreign.C.String      ( CString, newCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr           ( nullPtr )
import Foreign.Storable      ( poke )
import System.IO.Unsafe      ( unsafePerformIO )
--
import HROOT

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s


data Particle =
  Particle {
    ptlX :: CDouble
  , ptlY :: CDouble
  , ptlPx :: CDouble
  , ptlPy :: CDouble
  } deriving Show


generate :: TRandom -> Int -> IO [Particle]
generate tRandom n = do
  replicateM n $ do
    x <- gaus tRandom 0 0.5
    y <- gaus tRandom 0 0.5
    dx <- gaus tRandom 0 0.05
    dy <- gaus tRandom 0 0.05
    pure (Particle x y dx dy)

step1 :: Particle -> Particle
step1 (Particle x y dx dy) =
  Particle (x+dx) (y+dy) dx dy

step :: [Particle] -> [Particle]
step ps = map step1 ps


fillPtls :: TH2F -> [Particle] -> IO ()
fillPtls hist ps = mapM_ (\(Particle x y _ _) -> void $ fill2 hist x y) ps


main :: IO ()
main = do
  alloca $ \pargc -> do
    alloca $ \pargv -> do
      poke pargc (0::CInt)
      poke pargv (""::CString)
      gsys <- gSystem
      tapp <- newTApplication ("test"::CString) pargc pargv
      tcanvas <- newTCanvas ("Test"::CString) ("Test"::CString) 640 480

      h2 <- newTH2F ("test"::CString) ("test"::CString) 100 (-5.0) 5.0 100 (-5.0) 5.0
      setMarkerStyle h2 6
      setMarkerSize h2 5

      tRandom <- newTRandom 65535

      ps₀ <- generate tRandom 100

      draw h2 (""::CString)

      forkIO $ flip iterateM_ ps₀ $ \ps -> do
        threadDelay 1000000
        reset h2 (""::CString)
        fillPtls h2 ps
        let ps' = step ps
        pure ps'

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
