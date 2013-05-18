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
roofit_classes = [ rooPrintable 
                 , rooAbsArg, rooAbsReal, rooAbsLValue, rooAbsRealLValue, rooRealVar
                 , rooAbsData, rooDirItem, rooDataHist, rooAbsPdf, rooHistPdf
                 , rooAddPdf, rooPlot
                 , rooAbsCollection
                 , rooArgList 
                 ]  

  
rooPrintable :: Class 
rooPrintable = roofitclass "RooPrintable" [deletable] mempty 
                           [ Constructor [] ] 


rooAbsArg :: Class 
rooAbsArg = AbstractClass roofitcabal "RooAbsArg" [tNamed, rooPrintable] mempty 
            [ ] 

rooAbsReal :: Class
rooAbsReal = AbstractClass roofitcabal "RooAbsReal" [rooAbsArg] mempty 
             [ ] 

rooAbsLValue :: Class 
rooAbsLValue = AbstractClass roofitcabal "RooAbsLValue" [] mempty 
               [ ] 

rooAbsRealLValue :: Class 
rooAbsRealLValue = AbstractClass roofitcabal "RooAbsRealLValue" [rooAbsReal, rooAbsLValue] mempty 
                   [ ] 


rooRealVar :: Class 
rooRealVar = roofitclass "RooRealVar" [rooAbsRealLValue] mempty 
             [ Constructor [ cstring "name", cstring "title", double "value", double "minValue", double "maxValue", cstring "unit" ] 
             ] 



rooAbsData :: Class 
rooAbsData = AbstractClass roofitcabal "RooAbsData" [tNamed, rooPrintable] mempty 
             [] 

rooDirItem :: Class 
rooDirItem = roofitclass "RooDirItem" [] mempty 
             [ ] 

rooDataHist :: Class
rooDataHist = roofitclass "RooDataHist" [rooAbsData, rooDirItem] mempty 
              [ ] 

rooAbsPdf :: Class 
rooAbsPdf = AbstractClass roofitcabal "RooAbsPdf" [rooAbsReal] mempty 
            [ ] 

rooHistPdf :: Class 
rooHistPdf = roofitclass "RooHistPdf" [rooAbsPdf] mempty 
             [ ] 

rooAddPdf :: Class 
rooAddPdf = roofitclass "RooAddPdf" [rooAbsPdf] mempty 
            [ ] 

rooPlot :: Class 
rooPlot = roofitclass "RooPlot" [tNamed, rooPrintable] mempty 
          [ ] 


rooAbsCollection :: Class
rooAbsCollection = AbstractClass roofitcabal  "RooAbsCollection" [tObject, rooPrintable] mempty 
                   [ ] 

rooArgList :: Class
rooArgList = roofitclass "RooArgList" [rooAbsCollection] mempty 
             [ ] 





