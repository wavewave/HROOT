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
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
-- 
import HROOT.Data.Core.Class
import HROOT.Data.RooFit.Class

roostatscabal = Cabal { cabal_pkgname = "HROOT-RooFit-RooStats"
                     , cabal_cheaderprefix = "HROOTRooFitRooStats" 
                     , cabal_moduleprefix = "HROOT.RooFit.RooStats" 
                     } 

roostatsclass n ps ann fs = Class roostatscabal n ps ann Nothing fs

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
                   , simpleLikelihoodRatioTestStat
                   , ratioOfProfiledLikelihoodsTestStat 
                   , numEventsTestStat
                   ]  

roostats_topfunctions = [  ]


modelConfig :: Class 
modelConfig = roostatsclass "ModelConfig" [tNamed] mempty 
              [ Constructor [ cstring "name", cstring "title"] Nothing
              , NonVirtual (cppclass_ rooAbsPdf) "GetPdf" [] Nothing
              , NonVirtual (cppclass_ rooAbsPdf) "GetPriorPdf" [] Nothing
              , NonVirtual (cppclass_ rooAbsData) "GetProtoData" [] Nothing
              , NonVirtual (cppclass_ rooArgSet) "GetParametersOfInterest" [] Nothing 
              , NonVirtual (cppclass_ rooArgSet) "GetNuisanceParameters" [] Nothing
              , NonVirtual (cppclass_ rooArgSet) "GetConstraintParameters" [] Nothing
              , NonVirtual (cppclass_ rooArgSet) "GetObservables" [] Nothing
              , NonVirtual (cppclass_ rooArgSet) "GetConditionalObservables" [] Nothing
              , NonVirtual (cppclass_ rooArgSet) "GetSnapshot" [] Nothing
              , NonVirtual (cppclass_ rooWorkspace) "GetWS" [] Nothing
              , Virtual void_ "SetWorkspace" [ cppclassref rooWorkspace "ws" ] Nothing
              , Virtual void_ "SetPdf" [ cppclassref rooAbsPdf "pdf" ] Nothing
              , Virtual void_ "SetPriorPdf" [ cppclassref rooAbsPdf "pdf" ] Nothing
              , Virtual void_ "SetProtoData" [ cppclassref rooAbsData "dat"  ] Nothing
              , Virtual void_ "SetObservables" [ cppclassref rooArgSet "set" ] Nothing
              , Virtual void_ "SetConditionalObservables" [ cppclassref rooArgSet "set" ] Nothing
              , Virtual void_ "SetNuisanceParameters" [ cppclassref rooArgSet "set" ] Nothing
              , Virtual void_ "SetParametersOfInterest" [ cppclassref rooArgSet "set" ] Nothing

              ] 



testStatistic :: Class 
testStatistic = 
    AbstractClass roostatscabal "TestStatistic" [deletable] mempty Nothing
    [ Virtual double_ "Evaluate" [cppclassref rooAbsData "dat", cppclassref rooArgSet "paramsOfInterest"] Nothing
    -- , Virtual "GetVarName"
    ] 

profileLikelihoodTestStat :: Class 
profileLikelihoodTestStat = roostatsclass "ProfileLikelihoodTestStat" [testStatistic] mempty 
                            [ Constructor [] Nothing
                            ] 
  


intervalCalculator :: Class
intervalCalculator = 
    AbstractClass roostatscabal {- roostatsclass -} "IntervalCalculator" [] mempty Nothing
    [ Virtual (cppclass_ confInterval) "GetInterval" [] (Just "getInterval_IntervalCalculator")
    , Virtual double_ "Size" [] Nothing
    , Virtual double_ "ConfidenceLevel" [] (Just "confidenceLevel_IntervalCalculator")
    , Virtual void_ "SetData" [ cppclassref rooAbsData "dat" ] Nothing
    , Virtual void_ "SetModel" [ cppclassref modelConfig "model" ] Nothing
    , Virtual void_ "SetTestSize" [ double "size" ] Nothing
    , Virtual void_ "SetConfidenceLevel" [ double "cl" ] (Just "setConfidenceLevel_IntervalCalculator" )
    ]


confInterval :: Class
confInterval = roostatsclass "ConfInterval" [tNamed] mempty 
               [ Virtual bool_ "IsInInterval" [ cppclassref rooArgSet "set" ] Nothing
               , Virtual void_ "SetConfidenceLevel" [ double "cl" ] Nothing
               , Virtual double_ "ConfidenceLevel" [] Nothing
               , Virtual (cppclass_ rooArgSet) "GetParameters" [] Nothing
               ] 

-- unfortunately, some compilation error happened for hypoTestCalculator

hypoTestCalculator :: Class
hypoTestCalculator = 
    AbstractClass roostatscabal "HypoTestCalculator" [] mempty Nothing
    [ Virtual (cppclass_ hypoTestResult) "GetHypoTest" [] Nothing
    , Virtual void_ "SetNullModel" [ cppclassref modelConfig "model" ] Nothing
    , Virtual void_ "SetAlternateModel" [ cppclassref modelConfig "model" ] Nothing
    , Virtual void_ "SetData" [ cppclassref rooAbsData "dat" ] (Just "setData_hypoTestCalculator")
    , Virtual void_ "SetCommonModel" [ cppclassref modelConfig "model" ] Nothing
    ] 


combinedCalculator :: Class
combinedCalculator = 
    roostatsclass "CombinedCalculator" [intervalCalculator, hypoTestCalculator ] mempty 
    [ Virtual void_ "SetConditionalObservables" [cppclassref rooArgSet "set"] (Just "setConditionalObservables_CombinedCalculator")
    
    ] 

profileLikelihoodCalculator :: Class 
profileLikelihoodCalculator = 
    roostatsclass "ProfileLikelihoodCalculator" [combinedCalculator] mempty 
    [ Constructor [ cppclassref rooAbsData "data", cppclassref modelConfig "model" ] Nothing
    , Virtual void_ "SetNullParameters" [ cppclassref rooArgSet "set" ] Nothing
    ] 


likelihoodIntervalPlot :: Class
likelihoodIntervalPlot = 
    roostatsclass "LikelihoodIntervalPlot" [tNamed, rooPrintable] mempty 
    [ Constructor [ cppclass likelihoodInterval "theInterval" ] Nothing
    ] 

likelihoodInterval :: Class
likelihoodInterval = 
    roostatsclass "LikelihoodInterval" [confInterval] mempty 
    [ ] 

hypoTestResult :: Class 
hypoTestResult = 
    roostatsclass "HypoTestResult" [tNamed] mempty 
    [ Virtual double_ "NullPValue" [] Nothing
    , Virtual double_ "AlternatePValue" [] Nothing
    , Virtual double_ "CLb" [] Nothing
    , Virtual double_ "CLsplusb" [] Nothing
    , Virtual double_ "CLs" [] Nothing
    , Virtual double_ "Significance" [] Nothing 
    ] 

samplingDistribution :: Class
samplingDistribution = 
    roostatsclass "SamplingDistribution" [tNamed] mempty 
    [  ] 

simpleLikelihoodRatioTestStat :: Class
simpleLikelihoodRatioTestStat = 
    roostatsclass "SimpleLikelihoodRatioTestStat" [testStatistic] mempty 
    [  

    ] 

ratioOfProfiledLikelihoodsTestStat :: Class
ratioOfProfiledLikelihoodsTestStat = 
    roostatsclass "RatioOfProfiledLikelihoodsTestStat" [testStatistic] mempty 
    [  ] 


numEventsTestStat :: Class
numEventsTestStat = 
    roostatsclass "NumEventsTestStat" [testStatistic] mempty 
    [  ] 

