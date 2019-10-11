{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent    ( forkIO, threadDelay )
import Control.Monad         ( forever
                             , replicateM
                             , replicateM_
                             , void
                             , when
                             )
import Control.Monad.Loops   ( iterateUntilM )
import Data.Foldable         ( traverse_ )
import Data.Function         ( on )
import Data.IORef            ( newIORef, readIORef, modifyIORef' )
import Data.List             ( minimumBy )
import Data.String           ( IsString(fromString) )
import Data.Traversable      ( for, traverse )
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

sqr :: CDouble -> CDouble
sqr !x = x*x

data Box =
  Box {
    boxLowerLeft  :: (CDouble,CDouble)
  , boxUpperRight :: (CDouble,CDouble)
  }

data Particle =
  Particle {
    ptlId :: Int
  , ptlX :: CDouble
  , ptlY :: CDouble
  , ptlPx :: CDouble
  , ptlPy :: CDouble
  , ptlMarker :: TMarker
  }

mybox :: Box
mybox = Box (-5,-5) (5,5)

nParticles :: Int
nParticles = 500

neighborDist :: CDouble
neighborDist = 0.05

accelParam :: CDouble
accelParam = 0.01

plotRange :: (CDouble,CDouble)
plotRange = (0,0.15)

eps = 1e-10

mkUnitVector :: (CDouble,CDouble) -> (CDouble,CDouble)
mkUnitVector (x,y) =
  let n² = sqr x + sqr y
  in if n² < eps
     then (0,0)
     else let n = sqrt n²
          in (x/n,y/n)


generate :: TRandom -> Int -> IO [Particle]
generate tRandom n = do
  for [1..n] $ \i -> do
    x <- gaus tRandom 0 0.5
    y <- gaus tRandom 0 0.5
    r <- uniform tRandom 0 0.05
    θ <- uniform tRandom 0 (2*pi)
    let dx = r * cos θ
        dy = r * sin θ
    m1 <- newTMarker x y 3
    draw m1 (""::CString)
    pure (Particle i x y dx dy m1)

distanceSqrInS₁ :: (CDouble,CDouble) -> CDouble -> CDouble -> CDouble
distanceSqrInS₁ (xmin,xmax) x1 x2 =
  let l = xmax - xmin
      d1 = sqr (x1 - x2)
      d2 = sqr (x1 - x2 + l)
      d3 = sqr (x1 - x2 - l)
  in minimum [d1,d2,d3]

distanceSqr :: Box -> Particle -> Particle -> CDouble
distanceSqr (Box (xmin,ymin) (xmax,ymax)) p1 p2 =
  let (x1,y1) = (ptlX p1, ptlY p1)
      (x2,y2) = (ptlX p2, ptlY p2)
      dx² = distanceSqrInS₁ (xmin,xmax) x1 x2
      dy² = distanceSqrInS₁ (ymin,ymax) y1 y2
  in dx² + dy²

findNeighbor :: Box -> [Particle] -> Particle -> [Particle]
findNeighbor box ps p =
  let cond p' = ptlId p /= ptlId p' && distanceSqr box p p' < sqr neighborDist
  in filter cond ps

mkNeighborMap :: Box -> [Particle] -> [(Particle,[Particle])]
mkNeighborMap box ps = map (\p -> (p, findNeighbor box ps p)) ps

nearCoordInS₁ :: (CDouble,CDouble) -> CDouble -> CDouble -> CDouble
nearCoordInS₁ (xmin,xmax) x1 x2 =
  let l = xmax - xmin
      d1 = sqr (x1 - x2)
      d2 = sqr (x1 - x2 + l)
      d3 = sqr (x1 - x2 - l)
  in snd $ minimumBy (compare `on` fst) [(d1,x2),(d2,x2-l),(d3,x2+l)]

directionInTorus ::
     Box
  -> (CDouble,CDouble)
  -> (CDouble,CDouble)
  -> (CDouble,CDouble)
directionInTorus (Box (xmin,ymin) (xmax,ymax)) (x1,y1) (x2,y2) =
  let x2' = nearCoordInS₁ (xmin,xmax) x1 x2
      y2' = nearCoordInS₁ (ymin,ymax) y1 y2
      (rx,ry) = mkUnitVector (x2'-x1,y2'-y1)
  in (rx,ry)

neighborDir :: Box -> Particle -> Particle -> (CDouble,CDouble)
neighborDir box f t =
  directionInTorus box (ptlX f,ptlY f) (ptlX t,ptlY t)


addAccel :: (Particle, [(CDouble,CDouble)] )-> Particle
addAccel (Particle i x y dx dy m, vs) =
  let ddx = accelParam * sum (map fst vs)
      ddy = accelParam * sum (map snd vs)
  in Particle i x y (dx+ddx) (dy+ddy) m


format :: Box -> (Particle,[Particle]) -> (Int,[(CDouble,CDouble)])
format box (p,ps) = (ptlId p,map (neighborDir box p) ps)


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
step1 box (Particle i x y dx dy m) = do
  let (x',y') = updateInTorus box (x,y) (dx,dy)
  setX m x'
  setY m y'
  pure $ Particle i x' y' dx dy m

step :: Box -> [Particle] -> IO [Particle]
step box ps = do
  let nmap = mkNeighborMap box ps
      ps' = map addAccel $ map (\(x,xs) -> (x,map (neighborDir box x) xs)) nmap
  traverse (step1 box) ps'

release :: Particle -> IO ()
release (Particle _ _ _ _ _ m) = delete m


kineticEnergy :: Particle -> CDouble
kineticEnergy (Particle _ _ _ dx dy _) = 0.5 * (sqr dx + sqr dy)


velocity :: Particle -> CDouble
velocity (Particle _ _ _ dx dy _) = sqrt (sqr dx + sqr dy)

updateHist :: TH1F -> Particle -> IO ()
updateHist h1 p = void $ fill1 h1 (velocity p)


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

      h1 <- newTH1F ("velocity"::CString) ("velocity"::CString) 25 (fst plotRange) (snd plotRange)
      fn <- newTF1 ("MaxwellBoltzmann"::CString) ("[0]*x*exp(-x*x*[1])"::CString) (fst plotRange) (snd plotRange)

      cd tcanvas 0
      draw tpad1 (""::CString)
      cd tcanvas 0
      draw tpad2 (""::CString)

      cd tpad1 0
      tRandom <- newTRandom 65535
      ps₀ <- generate tRandom nParticles

      cd tpad2 0
      traverse_ (updateHist h1) ps₀
      draw h1 (""::CString)

      forkIO $ do
        threadDelay 2000000
        void $ flip (iterateUntilM (\(_,n) -> n > 100)) (ps₀,0) $ \(ps,n) -> do
          ps' <- step mybox ps
          reset h1 (""::CString)
          traverse_ (updateHist h1) ps'
          print (sum (map kineticEnergy ps'))
          pure (ps',n+1)
        void $ fit h1 fn (""::CString) (""::CString) (fst plotRange) (snd plotRange)


      forkIO $ forever $ do
        threadDelay (1000000 `div` 60) -- every 1/60 sec
        update tcanvas
        paint tcanvas (""::CString)

      forever $ do
        threadDelay (1000000 `div` 60) -- every 1/60 sec
        processEvents gsys
      traverse_ release ps₀
      delete tapp
