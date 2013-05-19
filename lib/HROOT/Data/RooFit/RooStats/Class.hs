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
                   , hypoTestResult 
                   -- , testStatSampler
                   , samplingDistribution
                   ]  


modelConfig :: Class 
modelConfig = roostatsclass "ModelConfig" [tNamed] mempty 
              [ Constructor [ cstring "name", cstring "title"] 
              , NonVirtual (cppclass_ rooAbsPdf) "GetPdf" [] 
              , NonVirtual (cppclass_ rooAbsPdf) "GetPriorPdf" [] 
              , NonVirtual (cppclass_ rooAbsData) "GetProtoData" [] 
              , NonVirtual (cppclass_ rooArgSet) "GetParametersOfInterest" [] 
              , NonVirtual (cppclass_ rooArgSet) "GetNuisanceParameters" [] 
              , NonVirtual (cppclass_ rooArgSet) "GetConstraintParameters" [] 
              , NonVirtual (cppclass_ rooArgSet) "GetObservables" [] 
              , NonVirtual (cppclass_ rooArgSet) "GetConditionalObservables" [] 
              , NonVirtual (cppclass_ rooArgSet) "GetSnapshot" [] 
              , NonVirtual (cppclass_ rooWorkspace) "GetWS" [] 
              , Virtual void_ "SetWorkspace" [ cppclassref rooWorkspace "ws" ] 
              , Virtual void_ "SetPdf" [ cppclassref rooAbsPdf "pdf" ] 
              , Virtual void_ "SetPriorPdf" [ cppclassref rooAbsPdf "pdf" ] 
              , Virtual void_ "SetProtoData" [ cppclassref rooAbsData "dat"  ]
              , Virtual void_ "SetObservables" [ cppclassref rooArgSet "set" ] 
              , Virtual void_ "SetParametersOfInterest" [ cppclassref rooArgSet "set" ] 

              ] 



testStatistic :: Class 
testStatistic = 
    AbstractClass roostatscabal "TestStatistic" [deletable] mempty 
    [ Virtual double_ "Evaluate" [cppclassref rooAbsData "dat", cppclassref rooArgSet "paramsOfInterest"]
     -- , GetVarName
    ] 

profileLikelihoodTestStat :: Class 
profileLikelihoodTestStat = roostatsclass "ProfileLikelihoodTestStat" [testStatistic] mempty 
                            [ Constructor [] ] 
  


intervalCalculator :: Class
intervalCalculator = 
    AbstractClass roostatscabal {- roostatsclass -} "IntervalCalculator" [] mempty 
    [ AliasVirtual (cppclass_ confInterval) "GetInterval" [] "getInterval_IntervalCalculator"
    , Virtual double_ "Size" [] 
    , AliasVirtual double_ "ConfidenceLevel" [] "confidenceLevel_IntervalCalculator"
    , Virtual void_ "SetData" [ cppclassref rooAbsData "dat" ] 
    , Virtual void_ "SetModel" [ cppclassref modelConfig "model" ]
    , Virtual void_ "SetTestSize" [ double "size" ] 
    , AliasVirtual void_ "SetConfidenceLevel" [ double "cl" ] "setConfidenceLevel_IntervalCalculator" 
    ]


confInterval :: Class
confInterval = roostatsclass "ConfInterval" [tNamed] mempty 
               [ Virtual bool_ "IsInInterval" [ cppclassref rooArgSet "set" ] 
               , Virtual void_ "SetConfidenceLevel" [ double "cl" ]
               , Virtual double_ "ConfidenceLevel" [] 
               , Virtual (cppclass_ rooArgSet) "GetParameters" [] 
               ] 

-- unfortunately, some compilation error happened for hypoTestCalculator

hypoTestCalculator :: Class
hypoTestCalculator = 
    AbstractClass roostatscabal "HypoTestCalculator" [] mempty 
    [ Virtual (cppclass_ hypoTestResult) "GetHypoTest" [] 
    , Virtual void_ "SetNullModel" [ cppclassref modelConfig "model" ] 
    , Virtual void_ "SetAlternateModel" [ cppclassref modelConfig "model" ] 
    , AliasVirtual void_ "SetData" [ cppclassref rooAbsData "dat" ] "setData_hypoTestCalculator"
    , Virtual void_ "SetCommonModel" [ cppclassref modelConfig "model" ]  
    ] 


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

hypoTestResult :: Class 
hypoTestResult = 
    roostatsclass "HypoTestResult" [tNamed] mempty 
    [ Virtual double_ "NullPValue" [] 
    , Virtual double_ "AlternatePValue" [] 
    , Virtual double_ "CLb" [] 
    , Virtual double_ "CLsplusb" [] 
    , Virtual double_ "CLs" [] 
    , Virtual double_ "Significance" [] 
    ] 

samplingDistribution :: Class
samplingDistribution = 
    roostatsclass "SamplingDistribution" [tNamed] mempty 
    [ 
    ] 
