module HROOT.Data.RooFit.Class where

import FFICXX.Generate.Code.Primitive ( bool, bool_
                                      , cppclass, cppclass_
                                      , cppclassref
                                      , cstring
                                      , double, double_
                                      , int, int_
                                      , void_
                                      )
import FFICXX.Generate.Type.Cabal     ( Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import HROOT.Data.Core.Class          ( deletable, tNamed, tObject )
import HROOT.Data.Hist.Class          ( tH1 )
import HROOT.Data.Math.Class          ( tRandom )


roofitcabal :: Cabal
roofitcabal =
  Cabal {
    cabal_pkgname            = CabalName "HROOT-RooFit"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTRooFit"
  , cabal_moduleprefix       = "HROOT.RooFit"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  }


roofitclass :: String -> [Class] -> [Function] -> Class
roofitclass n ps fs =
  Class {
      class_cabal      = roofitcabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    }


rooPrintable :: Class
rooPrintable = roofitclass "RooPrintable" [deletable]
                           [ Constructor [] Nothing ]


rooAbsArg :: Class
rooAbsArg = roofitclass "RooAbsArg" [tNamed, rooPrintable]
            [ ]

rooAbsReal :: Class
rooAbsReal = -- AbstractClass roofitcabal "RooAbsReal" [rooAbsArg] mempty
             roofitclass "RooAbsReal" [rooAbsArg]
             [ Virtual double_ "getVal" [ cppclass rooArgSet "set" ] Nothing
             , Virtual double_ "analyticalIntegral" [ int "code", cstring "rangeName" ] Nothing
             , Virtual (cppclass_ rooAbsReal) "createProfile" [ cppclassref rooArgSet "paramsOfInterest" ] Nothing
             , Virtual (cppclass_ rooAbsReal) "createIntegral" [ cppclassref rooArgSet "iset" ] Nothing
             , Virtual (cppclass_ rooPlot) "plotOn" [cppclass rooPlot "frame"] (Just "plotOn_rooAbsReal")
             , Virtual (cppclass_ tH1) "createHistogram" [cstring "varNameList"] Nothing
             ]

rooAbsLValue :: Class
rooAbsLValue =
  AbstractClass {
      class_cabal      = roofitcabal
    , class_name       = "RooAbsLValue"
    , class_parents    = []
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = []
    , class_vars       = []
    , class_tmpl_funcs = []
  }

rooAbsRealLValue :: Class
rooAbsRealLValue = -- AbstractClass roofitcabal "RooAbsRealLValue" [rooAbsReal, rooAbsLValue] mempty
                   roofitclass "RooAbsRealLValue" [rooAbsReal, rooAbsLValue]
                   [ Virtual (cppclass_ rooPlot) "frame" []  Nothing
                   ]


rooRealVar :: Class
rooRealVar = roofitclass "RooRealVar" [rooAbsRealLValue]
             [ Constructor [ cstring "name", cstring "title", double "value", double "minValue", double "maxValue", cstring "unit" ] Nothing
             , Virtual void_ "setVal" [ double "value" ] Nothing
             ]



rooAbsData :: Class
rooAbsData = roofitclass "RooAbsData" [tNamed, rooPrintable]
             [ Virtual (cppclass_ rooPlot) "plotOn" [ cppclass rooPlot "frame" ] Nothing
             , Virtual (cppclass_ rooPlot) "statOn" [ cppclass rooPlot "frame" ] Nothing
             , Virtual (cppclass_ tH1) "createHistogram" [ cstring "varNameList" ] (Just "createHistogram_RooAbsData")
              -- [cstring "name", cppclassref rooAbsRealLValue "xvar" ] "createHistogram_RooAbsData"
             , Virtual (cppclass_ rooAbsData) "get" [] (Just "get_RooAbsData")
             ]

rooDirItem :: Class
rooDirItem = roofitclass "RooDirItem" []
             [ ]

rooDataHist :: Class
rooDataHist = roofitclass "RooDataHist" [rooAbsData, rooDirItem]
              [ ]

rooAbsPdf :: Class
rooAbsPdf = -- AbstractClass roofitcabal "RooAbsPdf" [rooAbsReal] mempty
            -- [ ]
            roofitclass "RooAbsPdf" [rooAbsReal]
            [ Virtual (cppclass_ rooDataSet) "generate" [ cppclassref rooArgSet "whatVars", int "nEvent" ] Nothing
            , Virtual (cppclass_ rooDataSet) "generate" [ cppclassref rooArgSet "whatVars", cppclassref rooDataSet "prototype", int "nEvents", bool "verbose", bool "randProtoOrder", bool "resampleProto" ] (Just "generate_proto")
            , Virtual (cppclass_ rooDataHist) "generateBinned" [ cppclassref rooArgSet "whatVars", double "nEvents" {-, cppclassref rooCmdArg "arg1" -} ] Nothing
            , Virtual void_ "generateEvent" [ int "code" ] Nothing
            , Virtual int_ "getGenerator" [ cppclassref rooArgSet "directVars", cppclassref rooArgSet "generateVars", bool "staticInitOK" ] Nothing
            , Virtual (cppclass_ rooFitResult) "fitTo" [ cppclassref rooAbsData "dat" ] Nothing
            , Virtual (cppclass_ rooAbsReal) "createNLL" [ cppclassref rooAbsData "dat" ] Nothing
            , Virtual (cppclass_ rooAbsPdf) "createProjection" [cppclassref rooArgSet "iset" ] Nothing
            , Virtual (cppclass_ rooAbsReal) "createCdf" [cppclassref rooArgSet "iset" ] Nothing
            -- ExtendMode
            , Virtual double_ "expectedEvents" [cppclassref rooArgSet "nset" ] Nothing
            ]

rooHistPdf :: Class
rooHistPdf = roofitclass "RooHistPdf" [rooAbsPdf]
             [ ]

rooAddPdf :: Class
rooAddPdf = roofitclass "RooAddPdf" [rooAbsPdf]
            [ Constructor [ cstring "name", cstring "title", cppclassref rooAbsPdf "pdf1", cppclassref rooAbsPdf "pdf2", cppclassref rooAbsReal "coef1"] Nothing

            ]

rooPlot :: Class
rooPlot = roofitclass "RooPlot" [tNamed, rooPrintable]
          [ ]


rooAbsCollection :: Class
rooAbsCollection =
  roofitclass  "RooAbsCollection" [tObject, rooPrintable]
  [ Virtual bool_ "add" [cppclassref rooAbsArg "var", bool "silent" ] Nothing
  , Virtual bool_ "add" [cppclassref rooAbsCollection "list", bool "silent" ] (Just "addCollection")
  , Virtual (cppclass_ rooAbsArg) "addClone" [cppclassref rooAbsArg "var", bool "silent" ] Nothing
  , NonVirtual (cppclass_ rooAbsArg) "find" [cstring "name"] Nothing
  ]

rooArgList :: Class
rooArgList = roofitclass "RooArgList" [rooAbsCollection]
             [ ]

rooArgSet :: Class
rooArgSet = roofitclass "RooArgSet" [rooAbsCollection]
            [ Constructor [ cstring "name" ] Nothing
            , NonVirtual bool_ "setRealValue" [ cstring "name", double "newVal", bool "verbose" ] Nothing
            ]


rooRandom :: Class
rooRandom = roofitclass "RooRandom" []
            [ Static (cppclass_ tRandom) "randomGenerator" [] Nothing ]

rooWorkspace :: Class
rooWorkspace = roofitclass "RooWorkspace" [tNamed]
               [ Constructor [] Nothing
               , NonVirtual bool_ "defineSet" [ cstring "name", cstring "contentList" ] Nothing
               , NonVirtual bool_ "writeToFile" [ cstring "fileName", bool "recreate" ] Nothing
               , NonVirtual void_ "factory" [ cstring "expr" ] Nothing -- very unfortunate painful way
               , NonVirtual (cppclass_ rooAbsPdf) "pdf" [ cstring "name" ] Nothing
               , NonVirtual (cppclass_ rooAbsData) "data" [ cstring "name" ] Nothing
               , NonVirtual (cppclass_ rooRealVar) "var" [ cstring "name" ] Nothing
               , NonVirtual (cppclass_ rooArgSet) "set" [ cstring "name" ] Nothing
               , NonVirtual (cppclass_ rooAbsReal) "function" [cstring "name"] Nothing
               , NonVirtual (cppclass_ rooCategory) "cat" [cstring "name"] Nothing
               , NonVirtual (cppclass_ rooAbsCategory) "catfunc" [cstring "name"] Nothing
               , NonVirtual (cppclass_ rooAbsArg) "arg" [cstring "name"] Nothing
               -- components
               -- componentIterator
               , NonVirtual (cppclass_ tObject) "obj" [cstring "name"] Nothing
               ]

rooDataSet :: Class
rooDataSet = roofitclass "RooDataSet" [rooAbsData, rooDirItem]
             [ Virtual (cppclass_ rooAbsArg) "addColumn" [ cppclassref rooAbsArg "var", bool "adjustRange" ] Nothing
             ]

{-
rooCmdArg :: Class
rooCmdArg = roofitclass "RooCmdArg" [tNamed] mempty
            [ Static slecppclass_ rooCmdArg
            ]

-}

rooFitResult :: Class
rooFitResult = roofitclass "RooFitResult" [tNamed, rooPrintable, rooDirItem]
               [ ]

rooCategory :: Class
rooCategory = roofitclass "RooCategory" [rooAbsCategoryLValue]
              [ ]

rooAbsCategoryLValue :: Class
rooAbsCategoryLValue = roofitclass "RooAbsCategoryLValue" [rooAbsCategory, rooAbsLValue]
                       [ ]

rooAbsCategory :: Class
rooAbsCategory = roofitclass "RooAbsCategory" [rooAbsArg]
                 [ ]

rooGaussian :: Class
rooGaussian = roofitclass "RooGaussian" [rooAbsPdf]
              [ Constructor [ cstring "name", cstring "title", cppclassref rooAbsReal "x", cppclassref rooAbsReal "mean", cppclassref rooAbsReal "sigma" ] Nothing
              ]

rooGenericPdf :: Class
rooGenericPdf = roofitclass "RooGenericPdf" [rooAbsPdf]
              [ Constructor [ cstring "name", cstring "title", cstring "formula", cppclassref rooArgList "dependents" ] Nothing
              ]


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
                 -- , rooCmdArg
                 , rooFitResult
                 , rooCategory, rooAbsCategoryLValue, rooAbsCategory
                 , rooGaussian
                 , rooGenericPdf
                 ]

roofit_topfunctions :: [TopLevelFunction]
roofit_topfunctions = [ ]
