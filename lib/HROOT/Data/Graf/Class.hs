-- |
-- Module      : HROOT.Data.Graf.Class
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- 
-- License     : LGPL-2
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Graf.Class where

import Data.Monoid
--
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module
-- 
import HROOT.Data.Core.Class

histcabal = Cabal { cabal_pkgname = "HROOT-graf"
                  , cabal_cheaderprefix = "HROOTGraf" 
                  , cabal_moduleprefix = "HROOT.Graf" } 



tPad :: Class
tPad = 
  Class histcabal  "TPad" [tVirtualPad] mempty
  [ 
  ] 

tCanvas :: Class
tCanvas = Class histcabal "TCanvas" [tPad] mempty
          [ Constructor [cstring "name",cstring "title",int "ww",int "wh"] 
          ] 


graf_classes :: [Class]
graf_classes = 
  [ tPad, tCanvas ] 

