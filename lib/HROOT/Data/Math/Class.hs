-- |
-- Module      : HROOT.Data.Math.Class
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Math.Class where

import Data.Monoid
--
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
-- 
import HROOT.Data.Core.Class

mathcabal = Cabal { cabal_pkgname = "HROOT-math"
                  , cabal_cheaderprefix = "HROOTMath" 
                  , cabal_moduleprefix = "HROOT.Math" } 

mathclass = Class mathcabal

tRandom :: Class 
tRandom = 
  mathclass "TRandom" [tNamed] mempty
  [ Constructor [ int "seed" ] 
  , Virtual int_ "GetSeed" [] 
  , Virtual double_ "Gaus" [double "mean", double "sigma"]
  , Virtual void_ "SetSeed" [ int "seed" ] 
  , Virtual double_ "Uniform" [double "x1", double "x2"]
  ]       


math_classes :: [Class]
math_classes = 
  [ tRandom ] 

