{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeSynonymInstances #-}


module Main where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
--
import HROOT


data Point = Point { px :: CDouble, py :: CDouble, pz :: CDouble }

instance Storable Point where
  sizeOf _ = sizeOf (undefined :: CDouble) + sizeOf (undefined :: CDouble) + sizeOf (undefined :: CDouble)
  alignment _ = alignment (error "alignment" :: CDouble)
  peek ptr = Point <$> peek (castPtr ptr) 
                   <*> peek (castPtr ptr `plusPtr` sizeOf (error "peek" :: CDouble))
                   <*> peek (castPtr ptr `plusPtr` (2*sizeOf (error "peek" :: CDouble)))
  poke ptr Point {..} = poke (castPtr ptr) px 
                        >> poke (castPtr ptr `plusPtr` sizeOf (error "poke" :: CDouble)) py
                        >> poke (castPtr ptr `plusPtr` (2*sizeOf (error "poke" :: CDouble))) pz

main :: IO () 
main = do 
  tRandom <- newTRandom 65535
  let generator = gaus tRandom 0 2 
  alloca $ \(ptrpnt :: Ptr Point) -> do
    tree <- newTTree "T" "an example" 99
    br <- branch1 tree "point" (castPtr ptrpnt) "x/D:y/D:z/D" 32000
    let go = do p <- Point <$> generator <*> generator <*> generator
                poke ptrpnt p
                fillTree tree  
    replicateM_ 1000000 go      
    saveAs tree "treetest.root" ""
    delete tree


