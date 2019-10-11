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

data Box =
  Box {
    boxLowerLeft  :: (CDouble,CDouble)
  , boxUpperRight :: (CDouble,CDouble)
  }

data Particle =
  Particle {
    ptlX :: CDouble
  , ptlY :: CDouble
  , ptlPx :: CDouble
  , ptlPy :: CDouble
  , ptlMarker :: TMarker
  }

mybox :: Box
mybox = Box (-5,-5) (5,5)

nParticles :: Int
nParticles = 1000

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


fitInS₁ :: (CDouble,CDouble) -> CDouble -> CDouble
fitInS₁ (minx,maxx) x
  | x < minx   = maxx
  | x > maxx   = minx
  | otherwise  = x


updateInTorus ::
     Box
  -> (CDouble,CDouble)
  -> (CDouble,CDouble)
  -> (CDouble,CDouble)
updateInTorus (Box (minx,miny) (maxx,maxy)) (x,y) (dx,dy) =
  let x' = fitInS₁ (minx,maxx) (x + dx)
      y' = fitInS₁ (miny,maxy) (y + dy)
  in (x',y')

step1 :: Box -> Particle -> IO Particle
step1 box (Particle x y dx dy m) = do
  let (x',y') = updateInTorus box (x,y) (dx,dy)
  setX m x'
  setY m y'
  pure $ Particle x' y' dx dy m

step :: Box -> [Particle] -> IO [Particle]
step box ps = traverse (step1 box) ps

release :: Particle -> IO ()
release (Particle _ _ _ _ m) = delete m

updateHist :: TH1F -> Particle -> IO ()
updateHist h1 (Particle x _ _ _ _) = void $ fill1 h1 x

main :: IO ()
main = do
  alloca $ \pargc -> do
    alloca $ \pargv -> do
      poke pargc (0::CInt)
      poke pargv (""::CString)
      gsys <- gSystem
      tapp <- newTApplication ("test"::CString) pargc pargv
      tcanvas <- newTCanvas ("Test"::CString) ("Test"::CString) 1280 480
      tpad1 <- newTPad ("pad1"::CString) ("pad1"::CString) 0.05 0.05 0.49 0.95
      let Box (x1,y1) (x2,y2) = mybox
      range tpad1 x1 y1 x2 y2
      tpad2 <- newTPad ("pad2"::CString) ("pad2"::CString) 0.51 0.05 0.95 0.95

      h1 <- newTH1F ("histx"::CString) ("histx"::CString) 100 (-5.0) 5.0

      cd tcanvas 0
      draw tpad1 (""::CString)
      cd tcanvas 0
      draw tpad2 (""::CString)

      cd tpad1 0
      tRandom <- newTRandom 65535
      ps₀ <- generate tRandom nParticles

      cd tpad2 0
      draw h1 (""::CString)

      forkIO $ flip iterateM_ ps₀ $ \ps -> do
        threadDelay 10000
        ps' <- step mybox ps
        reset h1 (""::CString)
        traverse_ (updateHist h1) ps'
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
