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
import Data.Foldable         ( traverse_ )
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
import Data.String           ( IsString(fromString) )
import Data.Traversable      ( traverse )
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
  , ptlMarker :: TMarker
  }

generate :: TRandom -> Int -> IO [Particle]
generate tRandom n = do
  replicateM n $ do
    x <- gaus tRandom 0 0.5
    y <- gaus tRandom 0 0.5
    dx <- gaus tRandom 0 0.05
    dy <- gaus tRandom 0 0.05
    m1 <- newTMarker x y 3
    draw m1 (""::CString)

    pure (Particle x y dx dy m1)

step1 :: Particle -> IO Particle
step1 (Particle x y dx dy m) = do
  let x' = x + dx
      y' = y + dy
  setX m x
  setY m y
  pure $ Particle x' y' dx dy m

step :: [Particle] -> IO [Particle]
step ps = traverse step1 ps

release :: Particle -> IO ()
release (Particle _ _ _ _ m) = delete m

main :: IO ()
main = do
  alloca $ \pargc -> do
    alloca $ \pargv -> do
      poke pargc (0::CInt)
      poke pargv (""::CString)
      gsys <- gSystem
      tapp <- newTApplication ("test"::CString) pargc pargv
      tcanvas <- newTCanvas ("Test"::CString) ("Test"::CString) 640 480
      range tcanvas (-5.0) (-5.0) 5.0 5.0
      tRandom <- newTRandom 65535

      ps₀ <- generate tRandom 100

      forkIO $ flip iterateM_ ps₀ $ \ps -> do
        threadDelay 100000
        ps' <- step ps
        pure ps'

      forkIO $ forever $ do
        threadDelay (1000000 `div` 60) -- every 1/60 sec
        update tcanvas
        paint tcanvas (""::CString)


      forever $ do
        threadDelay (1000000 `div` 60) -- every 1/60 sec
        processEvents gsys
      traverse_ release ps₀
      delete tapp
