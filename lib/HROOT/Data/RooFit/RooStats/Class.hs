-----------------------------------------------------------------------------
-- |
-- Module      : HROOT.Data.RooFit.RooStats.Class
-- Copyright   : (c) 2013 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- ROOFIT/ROOSTATS
--
-----------------------------------------------------------------------------

module HROOT.Data.RooFit.RooStats.Class where

import Data.Monoid
-- 
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module
-- 
import HROOT.Data.Core.Class
import HROOT.Data.RooFit.Class

roostatscabal = Cabal { cabal_pkgname = "HROOT-RooFit-RooStats"
                     , cabal_cheaderprefix = "HROOTRooFitRooStats" 
                     , cabal_moduleprefix = "HROOT.RooFit.RooStats" 
                     } 

roostatsclass = Class roostatscabal 

roostats_classes :: [Class] 
roostats_classes = [ testStatistic
                   , profileLikelihoodTestStat
                   , modelConfig 
                   , hypoTestCalculator
                   , intervalCalculator
                   , confInterval 
                   , combinedCalculator 
                   , profileLikelihoodCalculator
                   , likelihoodIntervalPlot
                   , likelihoodInterval 
                   ]  


modelConfig :: Class 
modelConfig = roostatsclass "ModelConfig" [tNamed] mempty 
              [ Constructor [ cstring "name", cstring "title"] 
              , Virtual void_ "SetObservables" [ cppclassref rooArgSet "set" ] 
              , Virtual void_ "SetParametersOfInterest" [ cppclassref rooArgSet "set" ] 
              , Virtual void_ "SetPdf" [ cppclassref rooAbsPdf "pdf" ] 
              , Virtual void_ "SetWorkspace" [ cppclassref rooWorkspace "ws" ] 

              ] 



testStatistic :: Class 
testStatistic = AbstractClass roostatscabal "TestStatistic" [deletable] mempty 
                [ ] 

profileLikelihoodTestStat :: Class 
profileLikelihoodTestStat = roostatsclass "ProfileLikelihoodTestStat" [testStatistic] mempty 
                            [ Constructor [] ] 
  


intervalCalculator :: Class
intervalCalculator = AbstractClass roostatscabal {- roostatsclass -} "IntervalCalculator" [] mempty 
                     [ Virtual (cppclass_ confInterval) "GetInterval" []   
                     , Virtual void_ "SetConfidenceLevel" [ double "cl" ]
                     ]


confInterval :: Class
confInterval = roostatsclass "ConfInterval" [tNamed] mempty 
               [ 
               ] 

-- unfortunately, some compilation error happened for hypoTestCalculator

hypoTestCalculator :: Class
hypoTestCalculator = AbstractClass roostatscabal "HypoTestCalculator" [] mempty 
                     [] 


combinedCalculator :: Class
combinedCalculator = 
    roostatsclass "CombinedCalculator" [intervalCalculator, hypoTestCalculator ] mempty 
    [ 
    ] 

profileLikelihoodCalculator :: Class 
profileLikelihoodCalculator = 
    roostatsclass "ProfileLikelihoodCalculator" [combinedCalculator] mempty 
    [ Constructor [ cppclassref rooAbsData "data", cppclassref modelConfig "model" ] 
    ] 


likelihoodIntervalPlot :: Class
likelihoodIntervalPlot = 
    roostatsclass "LikelihoodIntervalPlot" [tNamed, rooPrintable] mempty 
    [ Constructor [ cppclass likelihoodInterval "theInterval" ] 
    ] 

likelihoodInterval :: Class
likelihoodInterval = 
    roostatsclass "LikelihoodInterval" [confInterval] mempty 
    [ ] 





