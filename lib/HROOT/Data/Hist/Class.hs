-- |
-- Module      : HROOT.Data.Core.ROOTsmall
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Hist.Class where

-- 
-- import Bindings.Cxx.Generate.Type.CType
-- import Bindings.Cxx.Generate.Type.Method
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module
-- 
import HROOT.Data.Core.Class

histcabal = Cabal { cabal_pkgname = "HROOT-hist"
                  , cabal_cheaderprefix = "HROOTHist" 
                  , cabal_moduleprefix = "HROOT.Hist" } 



tH1 :: Class
tH1 = 
  Class histcabal  "TH1" [deletable] 
  [ AliasVirtual int_ "Fill" [double "x"] "fill1"
  ] 

hist_classes :: [Class]
hist_classes = 
  [ tH1 ] 

