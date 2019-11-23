{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 ( ByteString )
import Data.Foldable         ( for_ )
import Data.Vector.Storable  ( Vector )
import qualified Data.Vector.Storable as VS
import Foreign.C.Types       ( CDouble, CInt )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable      ( poke )
--
import HROOT
import STD.Deletable.Interface (delete)


eps :: CDouble
eps = 0.001

main1 :: IO ()
main1 = do
  tcanvas <- newTCanvas ("Test"::ByteString) ("Test"::ByteString) 640 480
  gg <- newTGraph2D_

  let xs :: Vector CDouble
      xs = VS.generate (11*11) $ \k -> fromIntegral (k `mod` 11) - 5
      ys :: Vector CDouble
      ys = VS.generate (11*11) $ \k -> fromIntegral (k `div` 11) - 5
      zs :: Vector CDouble
      zs = VS.generate (11*11) $ \k ->
             let x = fromIntegral (k `mod` 11) - 5 + eps
                 y = fromIntegral (k `div` 11) - 5 + eps
             in (sin x / x) * (sin y / y) + 0.2

  VS.unsafeWith xs $ \pxs ->
    VS.unsafeWith ys $ \pys ->
      VS.unsafeWith zs $ \pzs -> do
        g2 <- newTGraph2D (11*11) pxs pys pzs
        draw g2 ("surf1"::ByteString)
        saveAs tcanvas ("graph2d.pdf"::ByteString) (""::ByteString)
        saveAs tcanvas ("graph2d.jpg"::ByteString) (""::ByteString)
        saveAs tcanvas ("graph2d.png"::ByteString) (""::ByteString)
        delete g2


  delete tcanvas

main2 :: IO ()
main2 = do
  tcanvas <- newTCanvas ("Graph2DTest"::ByteString) ("Graph 2D Test 2"::ByteString) 640 480
  gg <- newTGraph2D_

  let nX = 11
      nY = 11
      nXY = nX*nY

  setN gg nXY

  for_ [0..nXY-1] $ \k -> do
    let x = fromIntegral (k `mod` 11) - 5 + eps
        y = fromIntegral (k `div` 11) - 5 + eps
        z = (sin x / x) * (sin y / y) + 0.2
    setPointXYZ gg k x y z

  draw gg ("surf1"::ByteString)
  saveAs tcanvas ("graph2d2.pdf"::ByteString) (""::ByteString)
  saveAs tcanvas ("graph2d2.jpg"::ByteString) (""::ByteString)
  saveAs tcanvas ("graph2d2.png"::ByteString) (""::ByteString)
  delete gg
  delete tcanvas

main :: IO ()
main = main2
