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
import HROOT.Data.Math.Class

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
                 , rooArgList, rooArgSet
                 , rooRandom
                 , rooWorkspace
                 , rooDataSet
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
                   [ Virtual (cppclass_ rooPlot) "frame" []  
                   ] 


rooRealVar :: Class 
rooRealVar = roofitclass "RooRealVar" [rooAbsRealLValue] mempty 
             [ Constructor [ cstring "name", cstring "title", double "value", double "minValue", double "maxValue", cstring "unit" ] 
             ] 



rooAbsData :: Class 
rooAbsData = roofitclass "RooAbsData" [tNamed, rooPrintable] mempty 
             [ Virtual (cppclass_ rooPlot) "plotOn" [ cppclass rooPlot "frame" ] 
             , Virtual (cppclass_ rooPlot) "statOn" [ cppclass rooPlot "frame" ] 
             ] 

rooDirItem :: Class 
rooDirItem = roofitclass "RooDirItem" [] mempty 
             [ ] 

rooDataHist :: Class
rooDataHist = roofitclass "RooDataHist" [rooAbsData, rooDirItem] mempty 
              [ ] 

rooAbsPdf :: Class 
rooAbsPdf = -- AbstractClass roofitcabal "RooAbsPdf" [rooAbsReal] mempty 
            -- [ ] 
            roofitclass "RooAbsPdf" [rooAbsReal] mempty 
            [ Virtual (cppclass_ rooDataSet) "generate" [ cppclassref rooArgSet "whatVars", int "nEvent" ] 
            ] 

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

rooArgSet :: Class 
rooArgSet = roofitclass "RooArgSet" [rooAbsCollection] mempty 
            [ ] 


rooRandom :: Class 
rooRandom = roofitclass "RooRandom" [] mempty 
            [ Static (cppclass_ tRandom) "randomGenerator" [] ] 

rooWorkspace :: Class 
rooWorkspace = roofitclass "RooWorkspace" [tNamed] mempty 
               [ Constructor [] 
               , Virtual void_ "factory" [ cstring "expr" ]  -- very unfortunate painful way
               , Virtual bool_ "defineSet" [ cstring "name", cstring "contentList" ]  
               , Virtual (cppclass_ rooAbsPdf) "pdf" [ cstring "name" ] 
               , Virtual (cppclass_ rooArgSet) "set" [ cstring "name" ] 
               , Virtual (cppclass_ rooRealVar) "var" [ cstring "name" ] 
               ] 

rooDataSet :: Class
rooDataSet = roofitclass "RooDataSet" [rooAbsData, rooDirItem] mempty 
             [] 



