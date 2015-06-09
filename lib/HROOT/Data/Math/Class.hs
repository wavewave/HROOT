-- |
-- Module      : HROOT.Data.Math.Class
-- Copyright   : (c) 2011-2013, 2015 Ian-Woo Kim
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

mathclass n ps ann fs = Class mathcabal n ps ann Nothing fs

tRandom :: Class 
tRandom = 
  mathclass "TRandom" [tNamed] mempty
  [ Constructor [ int "seed" ] Nothing
  , Virtual int_ "GetSeed" [] Nothing
  , Virtual double_ "Gaus" [double "mean", double "sigma"] Nothing
  , Virtual void_ "SetSeed" [ int "seed" ] Nothing
  , Virtual double_ "Uniform" [double "x1", double "x2"] Nothing
  ]       


-- rootFitFitResult :: Class
-- rootFitFitResult = 
--  mathclass "ROOT::Fit::FitResult" [] mempty
--  [ ]

math_classes :: [Class]
math_classes = 
  [ tRandom {- , rootFitFitResult -} ] 

math_topfunctions = [] 
