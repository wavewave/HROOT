-----------------------------------------------------------------------------
-- |
-- Module      : HROOT.Data.RooFit.Class
-- Copyright   : (c) 2013 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- ROOFIT
--
-----------------------------------------------------------------------------

module HROOT.Data.RooFit.Class where

import Data.Monoid
-- 
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module
-- 
import HROOT.Data.Core.Class

roofitcabal = Cabal { cabal_pkgname = "HROOT-RooFit"
                     , cabal_cheaderprefix = "HROOTRooFit" 
                     , cabal_moduleprefix = "HROOT.RooFit" 
                     } 

roofitclass = Class roofitcabal 

roofit_classes :: [Class] 
roofit_classes = [ rooPrintable ]  

  
rooPrintable :: Class 
rooPrintable = roofitclass "RooPrintable" [deletable] mempty 
                           [ Constructor [] ] 












