module HROOT.Data.RooFit.RooStats.Class where


import FFICXX.Generate.Code.Primitive ( bool_
                                      , cppclass, cppclass_
                                      , cppclassref
                                      , cstring
                                      , double, double_
                                      , void_
                                      )
import FFICXX.Generate.Type.Cabal     ( Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import HROOT.Data.Core.Class          ( deletable, tNamed )
import HROOT.Data.RooFit.Class        ( rooAbsData, rooAbsPdf, rooArgSet, rooPrintable
                                      , rooWorkspace
                                      )


roostatscabal :: Cabal
roostatscabal =
  Cabal {
    cabal_pkgname            = CabalName "HROOT-RooFit-RooStats"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTRooFitRooStats"
  , cabal_moduleprefix       = "HROOT.RooFit.RooStats"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "HROOT-core"
                               , CabalName "HROOT-RooFit" ]
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  }

roostatsclass :: String -> [Class] -> [Function] -> Class
roostatsclass n ps fs =
  Class {
      class_cabal      = roostatscabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    }



modelConfig :: Class
modelConfig = roostatsclass "ModelConfig" [tNamed]
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
  AbstractClass {
    class_cabal      = roostatscabal
  , class_name       = "TestStatistic"
  , class_parents    = [deletable]
  , class_protected  = Protected []
  , class_alias      = Nothing
  , class_funcs      = [ Virtual double_ "Evaluate" [cppclassref rooAbsData "dat", cppclassref rooArgSet "paramsOfInterest"] Nothing
    -- , Virtual "GetVarName"
                       ]
  , class_vars       = []
  , class_tmpl_funcs = []
  }

profileLikelihoodTestStat :: Class
profileLikelihoodTestStat =
  roostatsclass "ProfileLikelihoodTestStat" [testStatistic]
  [ Constructor [] Nothing
  ]

intervalCalculator :: Class
intervalCalculator =
  AbstractClass {
    class_cabal      = roostatscabal
  , class_name       = "IntervalCalculator"
  , class_parents    = []
  , class_protected  = Protected []
  , class_alias      = Nothing
  , class_funcs      =
      [ Virtual (cppclass_ confInterval) "GetInterval" [] (Just "getInterval_IntervalCalculator")
      , Virtual double_ "Size" [] Nothing
      , Virtual double_ "ConfidenceLevel" [] (Just "confidenceLevel_IntervalCalculator")
      , Virtual void_ "SetData" [ cppclassref rooAbsData "dat" ] Nothing
      , Virtual void_ "SetModel" [ cppclassref modelConfig "model" ] Nothing
      , Virtual void_ "SetTestSize" [ double "size" ] Nothing
      , Virtual void_ "SetConfidenceLevel" [ double "cl" ] (Just "setConfidenceLevel_IntervalCalculator" )
      ]

  , class_vars       = []
  , class_tmpl_funcs = []
  }

confInterval :: Class
confInterval =
  roostatsclass "ConfInterval" [tNamed]
  [ Virtual bool_ "IsInInterval" [ cppclassref rooArgSet "set" ] Nothing
  , Virtual void_ "SetConfidenceLevel" [ double "cl" ] Nothing
  , Virtual double_ "ConfidenceLevel" [] Nothing
  , Virtual (cppclass_ rooArgSet) "GetParameters" [] Nothing
  ]

-- unfortunately, some compilation error happened for hypoTestCalculator

hypoTestCalculator :: Class
hypoTestCalculator =
  AbstractClass {
    class_cabal      = roostatscabal
  , class_name       = "HypoTestCalculator"
  , class_parents    = []
  , class_protected  = Protected []
  , class_alias      = Nothing
  , class_funcs      =
      [ Virtual (cppclass_ hypoTestResult) "GetHypoTest" [] Nothing
      , Virtual void_ "SetNullModel" [ cppclassref modelConfig "model" ] Nothing
      , Virtual void_ "SetAlternateModel" [ cppclassref modelConfig "model" ] Nothing
      , Virtual void_ "SetData" [ cppclassref rooAbsData "dat" ] (Just "setData_hypoTestCalculator")
      , Virtual void_ "SetCommonModel" [ cppclassref modelConfig "model" ] Nothing
      ]
  , class_vars       = []
  , class_tmpl_funcs = []
  }

combinedCalculator :: Class
combinedCalculator =
  roostatsclass "CombinedCalculator" [intervalCalculator, hypoTestCalculator ]
  [ Virtual void_ "SetConditionalObservables" [cppclassref rooArgSet "set"] (Just "setConditionalObservables_CombinedCalculator")
  ]

profileLikelihoodCalculator :: Class
profileLikelihoodCalculator =
  roostatsclass "ProfileLikelihoodCalculator" [combinedCalculator]
  [ Constructor [ cppclassref rooAbsData "data", cppclassref modelConfig "model" ] Nothing
  , Virtual void_ "SetNullParameters" [ cppclassref rooArgSet "set" ] Nothing
  ]


likelihoodIntervalPlot :: Class
likelihoodIntervalPlot =
  roostatsclass "LikelihoodIntervalPlot" [tNamed, rooPrintable]
  [ Constructor [ cppclass likelihoodInterval "theInterval" ] Nothing
  ]

likelihoodInterval :: Class
likelihoodInterval =
  roostatsclass "LikelihoodInterval" [confInterval]
  [ ]

hypoTestResult :: Class
hypoTestResult =
  roostatsclass "HypoTestResult" [tNamed]
  [ Virtual double_ "NullPValue" [] Nothing
  , Virtual double_ "AlternatePValue" [] Nothing
  , Virtual double_ "CLb" [] Nothing
  , Virtual double_ "CLsplusb" [] Nothing
  , Virtual double_ "CLs" [] Nothing
  , Virtual double_ "Significance" [] Nothing
  ]

samplingDistribution :: Class
samplingDistribution =
  roostatsclass "SamplingDistribution" [tNamed]
  []

simpleLikelihoodRatioTestStat :: Class
simpleLikelihoodRatioTestStat =
  roostatsclass "SimpleLikelihoodRatioTestStat" [testStatistic]
  []

ratioOfProfiledLikelihoodsTestStat :: Class
ratioOfProfiledLikelihoodsTestStat =
  roostatsclass "RatioOfProfiledLikelihoodsTestStat" [testStatistic]
  []


numEventsTestStat :: Class
numEventsTestStat =
  roostatsclass "NumEventsTestStat" [testStatistic]
  []


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

roostats_topfunctions :: [TopLevelFunction]
roostats_topfunctions = []
