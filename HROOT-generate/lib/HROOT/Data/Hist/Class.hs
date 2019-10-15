{-# LANGUAGE OverloadedStrings #-}

module HROOT.Data.Hist.Class where

import FFICXX.Generate.Code.Primitive ( bool    , bool_
                                      , cppclass, cppclass_
                                      , cstring
                                      , double  , double_
                                      , doublep
                                      , float   , float_
                                      , int     , int_
                                      , self_
                                      , short   , short_
                                      , void_
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import HROOT.Data.Core.Class          ( modImports
                                      , tArrayC, tArrayD, tArrayF, tArrayI, tArrayS
                                      , tAtt3D
                                      , tAttAxis
                                      , tAttFill
                                      , tAttLine
                                      , tAttMarker
                                      , tDirectory
                                      , tNamed
                                      , tObjArray
                                      , tObject
                                      )

histcabal :: Cabal
histcabal =
  Cabal {
    cabal_pkgname            = CabalName "HROOT-hist"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTHist"
  , cabal_moduleprefix       = "HROOT.Hist"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "HROOT-core" ]
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Custom [CabalName "Cabal", CabalName "base", CabalName "process"]
  }

histclass :: String -> [Class] -> ProtectedMethod -> [Function] -> Class
histclass n ps protect fs =
  Class {
    class_cabal      = histcabal
  , class_name       = n
  , class_parents    = ps
  , class_protected  = protect
  , class_alias      = Nothing
  , class_funcs      = fs
  , class_vars       = []
  , class_tmpl_funcs = []
  }


----------------
-- starting A --
----------------

tAxis :: Class
tAxis =
  histclass "TAxis" [tNamed, tAttAxis] (Protected [])
  [ Constructor [int "nbins", double "xmin", double "xmax"] Nothing
  , Virtual int_ "FindBin" [double "x"] (Just "findBinTAxis")
  , Virtual int_ "FindFixBin" [double "x"] (Just "findFixBinTAxis")
  , Virtual double_ "GetBinCenter" [int "bin"]  (Just "getBinCenterTAxis")
  , Virtual double_ "GetBinCenterLog" [int "bin"] Nothing
  -- , Virtual double_ "GetBinLabel" [int "bin"] Nothing
  , Virtual double_ "GetBinUpEdge" [int "bin"] Nothing
  -- GetCenter
  , NonVirtual bool_ "GetCenterLabels" [] Nothing
  , NonVirtual bool_ "GetCenterTitle" [] Nothing
  , NonVirtual int_ "GetFirst" [] Nothing
  , NonVirtual int_ "GetLast" [] Nothing
  -- GetLowEdge
  , NonVirtual int_ "GetNbins" [] Nothing
  , NonVirtual (cppclass_ tArrayD) "GetXbins" [] Nothing
  , NonVirtual double_ "GetXmax" [] Nothing
  , NonVirtual double_ "GetXmin" [] Nothing
  , Virtual void_ "SetTimeDisplay" [ int "value" ] Nothing
  , Virtual void_ "SetTimeFormat" [ cstring "format" ] Nothing
  , Virtual void_ "SetTimeOffset" [double "toffset", cstring "option"] Nothing
  ]

----------------
-- starting F --
----------------

{-
tF1 :: Class
tF1 =
  histclass "TF1" [tAttLine, tAttFill, tAttMarker] mempty
  [ Constructor [cstring "name",cstring "formula",double "xmin",double "xmax"] Nothing
  ]
-}

tF1 :: Class
tF1 =
  histclass "TF1" [tAttLine, tAttFill, tAttMarker] (Protected [])
  [ Constructor [cstring "name",cstring "formula",double "xmin",double "xmax"] Nothing
  -- Browse
  , Virtual double_ "Derivative" [double "x", doublep "params", double "epsilon"] Nothing
  , Virtual double_ "Derivative2" [double "x", doublep "params", double "epsilon"] Nothing
  , Virtual double_ "Derivative3" [double "x", doublep "params", double "epsilon"] Nothing
  , Static  double_ "DerivativeError" [] Nothing
  -- DerivativeError
  , Virtual self_ "DrawCopy" [cstring "option"] (Just "drawCopyTF1")
  , Virtual (cppclass_ tObject) "DrawDerivative" [cstring "option"] Nothing
  , Virtual (cppclass_ tObject) "DrawIntegral" [cstring "option"] Nothing
  -- , Virtual void_ "DrawF1" [cstring "formula", double "xmin", double "xmax", cstring "option"] Nothing
  , Virtual void_ "FixParameter" [int "ipar", double "value"] Nothing
  , NonVirtual double_ "GetChisquare" [] Nothing
  , NonVirtual (cppclass_ tH1)  "GetHistogram" [] Nothing
  , Virtual double_ "GetMaximum" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] (Just "getMaximumTF1")
  , Virtual double_ "GetMinimum" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] (Just "getMinimumTF1")
  , Virtual double_ "GetMaximumX" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] Nothing
  , Virtual double_ "GetMinimumX" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] Nothing
  , Virtual int_ "GetNDF" [] Nothing
  , Virtual int_ "GetNpx" [] Nothing
  -- GetMethodCall
  , Virtual int_ "GetNumberFreeParameters" [] Nothing
  , Virtual int_ "GetNumberFitPoints" [] Nothing
  , NonVirtual (cppclass_ tObject) "GetParent" [] Nothing
  , Virtual double_ "GetParError" [int "ipar"] Nothing
  -- GetParErrors
  -- GetParLiits
  , Virtual double_ "GetProb" [] Nothing
  , Virtual int_ "GetQuantiles" [int "nprobSum", doublep "q", doublep "probSum"] (Just "getQuantilesTF1")
  , Virtual double_ "GetRandom" [double "xmin", double "xmax"] (Just "getRandomTF1")
  -- GetRange
  , Virtual double_ "GetSave" [doublep "x"] Nothing
  , Virtual double_ "GetX" [double "y", double "xmin", double "xmax", double "epsilon", int "maxiter"] Nothing
  , Virtual double_ "GetXmin" [] Nothing
  , Virtual double_ "GetXmax" [] Nothing
  , NonVirtual (cppclass_ tAxis) "GetXaxis" [] Nothing
  , NonVirtual (cppclass_ tAxis) "GetYaxis" [] Nothing
  , NonVirtual (cppclass_ tAxis) "GetZaxis" [] Nothing
  , Virtual double_ "GradientPar" [int "ipar", doublep "x", double "eps"] Nothing
  , Virtual void_ "InitArgs" [doublep "x", doublep "params"] Nothing
  , Static  void_ "InitStandardFunctions" [] Nothing
  , Virtual double_ "Integral" [double "a", double "b", double "epsilon"] (Just "IntegralTF1")
  , Virtual double_ "IntegralError" [double "a", double "b", doublep "params", doublep "covmat", double "epsilon"] Nothing
  , Virtual double_ "IntegralFast" [int "num", doublep "x", doublep "w", double "a", double "b", doublep "params", double "epsilon"] Nothing
  -- IntegralMultiple
  , Virtual bool_ "IsInside" [doublep "x"] Nothing
  , Virtual void_ "ReleaseParameter" [int "ipar"] Nothing
  , Virtual void_ "SetChisquare" [double "chi2"] Nothing
  -- SetFitResult
  , Virtual void_ "SetMaximum" [double "maximum"] (Just "setMaximumTF1")
  , Virtual void_ "SetMinimum" [double "minimum"] (Just "setMinimumTF1")
  , Virtual void_ "SetNDF" [int "ndf"] Nothing
  , Virtual void_ "SetNumberFitPoints" [int "npfits"] Nothing
  , Virtual void_ "SetNpx" [int "npx"] Nothing
  , Virtual void_ "SetParError" [int "ipar", double "error"] Nothing
  , Virtual void_ "SetParErrors" [doublep "errors"] Nothing
  , Virtual void_ "SetParLimits" [int "ipar", double "parmin", double "parmax"] Nothing
  , Virtual void_ "SetParent" [cppclass tObject "parent"] Nothing
  , Virtual void_ "SetRange" [double "xmin", double "xmax"] (Just "setRange1")
  , Virtual void_ "SetRange" [double "xmin", double "xmax", double "ymin", double "ymax"] (Just "setRange2")
  , Virtual void_ "SetRange" [double "xmin", double "xmax", double "ymin", double "ymax", double "zmin", double "zmax"] (Just "setRange3")
  , Virtual void_ "SetSavedPoint" [int "point", double "value"] Nothing

  , Static  (cppclass_ tF1) "GetCurrent" [] Nothing
  , Static  void_ "AbsValue" [bool "reject"] Nothing
  , Static  void_ "RejectPoint" [bool "reject"] Nothing
  , Static  bool_ "RejectedPoint" [] Nothing
  , Static  void_ "SetCurrent" [cppclass tF1 "f1"] Nothing
  -- RejectPoint
  -- RejectedPoint
  -- SetCurrent
  , Virtual double_ "Moment" [double "n", double "a", double "b", doublep "params", double "epsilon"] Nothing
  , Virtual double_ "CentralMoment" [double "n", double "a", double "b", doublep "params", double "epsilon"] Nothing
  , Virtual double_ "Mean" [double "a", double "b", doublep "params", double "epsilon"] Nothing
  , Virtual double_ "Variance" [double "a", double "b", doublep "params", double "epsilon"] Nothing
  , Static  void_ "CalcGaussLegendreSamplingPoints" [int "num", doublep "x", doublep "w", double "eps"] Nothing
  ]



tFitResult :: Class
tFitResult =
    histclass "TFitResult" [tNamed] (Protected []) -- rootFitFitResult
    [ ]


tFitResultPtr :: Class
tFitResultPtr =
    histclass "TFitResultPtr" [] (Protected [])
    [ NonVirtual (cppclass_ tFitResult) "Get" [] Nothing
    ]


{-
tFormula :: Class
tFormula = histclass "TFormula" [tNamed] mempty
           [ Constructor [cstring "name", cstring "formula"] Nothing
           , NonVirtual void_ "Optimize" [] Nothing
           -- Analyze
           -- AnalyzeFunction
           -- , Virtual int_ "Compile" [cstring "expression"] Nothing
           , Virtual void_ "Clear" [cstring "option"] Nothing
           -- DefinedString
           , Virtual double_ "DefinedValue" [int "code"] Nothing
           -- DefinedVariable
           , Virtual double_ "Eval" [double "x", double "y", double "z", double "t"] Nothing
           -- , Virtual double_ "EvalParOld" [doublep "x", doublep "params"] Nothing
           , Virtual double_ "EvalPar" [doublep "x", doublep "params"] Nothing
           -- GetLinearPart
           , Virtual int_ "GetNdim" [] Nothing
           , Virtual int_ "GetNpar" [] Nothing
           , Virtual int_ "GetNumber" [] Nothing
           -- GetExpFormula
           , NonVirtual double_ "GetParameter"    [cstring "name" ] Nothing
           -- GetParameters
           -- GetParName
           , Virtual int_   "GetParNumber" [cstring "name"] Nothing
           , Virtual bool_  "IsLinear" [] Nothing
           -- , Virtual bool_  "IsNormalized" [] Nothing
           -- ProcessLinear
           -- , Virtual void_  "SetNumber" [int "number"] Nothing
           , Virtual void_  "SetParameter" [cstring "name", double "parvalue"] Nothing
           , Virtual void_  "SetParameters" [doublep "params"] Nothing
           , Virtual void_  "SetParName"  [int "ipar", cstring "name"] Nothing
           , Virtual void_  "SetParNames" [cstring "name0", cstring "name1", cstring "name2"
                                          ,cstring "name3", cstring "name4", cstring "name5"
                                          ,cstring "name6", cstring "name7", cstring "name8"
                                          ,cstring "name9", cstring "name10" ] Nothing
           , Virtual void_  "Update" [] Nothing
           -- SetMaxima
           ]

-}

----------------
-- starting G --
----------------


tGraph :: Class
tGraph =
  histclass "TGraph" [tNamed, tAttLine, tAttFill, tAttMarker] (Protected [])
  [ Constructor [int "n", doublep "x", doublep "y"] Nothing
  , Virtual void_ "Apply" [cppclass tF1 "f"] Nothing
  , Virtual double_ "Chisquare" [cppclass tF1 "f1"] Nothing
  -- CompareArg
  -- CompareX
  -- CompareY
  -- CompareRadius
  -- ComputeRange
  , Virtual void_ "DrawGraph" [int "n", doublep "x", doublep "y", cstring "option"] Nothing
  , Virtual void_ "DrawPanel" [] (Just "drawPanelTGraph")
  -- Eval
  , Virtual void_ "Expand" [int "newsize", int "step"] Nothing
  -- Fit
  , Virtual void_ "FitPanel" [] (Just "FitPanelTGraph")
  , NonVirtual bool_ "GetEditable" [] Nothing
  , NonVirtual (cppclass_ tF1) "GetFunction" [cstring "name"] Nothing
  , NonVirtual (cppclass_ tH1F) "GetHistogram" [] Nothing
  -- , NonVirtual (cppclass_ "TList") "GetListOfFunctions" []
  , Virtual double_ "GetCorrelationFactor" [] (Just "getCorrelationFactorTGraph")
  , Virtual double_ "GetCovariance" [] (Just "getCovarianceTGraph")
  , Virtual double_ "GetMean" [int "axis"] (Just "getMeanTGraph")
  , Virtual double_ "GetRMS" [int "axis"] (Just "getRMSTGraph")
  , NonVirtual int_ "GetMaxSize" [] Nothing
  , NonVirtual int_ "GetN" [] Nothing
  , Virtual double_ "GetErrorX" [int "bin"] Nothing
  , Virtual double_ "GetErrorY" [int "bin"] Nothing
  , Virtual double_ "GetErrorXhigh" [int "bin"] Nothing
  , Virtual double_ "GetErrorXlow" [int "bin"] Nothing
  , Virtual double_ "GetErrorYhigh" [int "bin"] Nothing
  , Virtual double_ "GetErrorYlow" [int "bin"] Nothing
  -- GetX
  -- GetY
  -- GetEX
  -- GetEY
  -- omit..
  , NonVirtual double_ "GetMaximum" [] Nothing
  , NonVirtual double_ "GetMinimum" [] Nothing
  , NonVirtual (cppclass_ tAxis) "GetXaxis" [] Nothing
  , NonVirtual (cppclass_ tAxis) "GetYaxis" [] Nothing
  -- GetPoint
  , Virtual void_ "InitExpo" [double "xmin", double "xmax"] Nothing
  , Virtual void_ "InitGaus" [double "xmin", double "xmax"] Nothing
  , Virtual void_ "InitPolynom" [double "xmin", double "xmax"] Nothing
  , Virtual int_ "InsertPoint" [] Nothing
  , Virtual double_ "Integral" [int "first", int "last"] (Just "integralTGraph")
  , Virtual bool_ "IsEditable" [] Nothing
  , Virtual int_ "IsInside" [double "x", double "y"] (Just "isInsideTGraph")
  , Virtual void_ "LeastSquareFit" [int "m", doublep "a", double "xmin", double "xmax"] Nothing
  -- LeastSquareLinearFit
  , NonVirtual void_ "PaintGraph" [int "npoints", doublep "x", doublep "y", cstring "chopt"] Nothing
  , NonVirtual void_ "PaintGrapHist" [int "npoints", doublep "x", doublep "y", cstring "chopt"] Nothing
  , Virtual void_ "PaintStats" [cppclass tF1 "fit"] Nothing
  , Virtual int_ "RemovePoint" [int "ipoint"] Nothing
  , Virtual void_ "SetEditable" [bool "editable"] Nothing
  , Virtual void_ "SetHistogram" [cppclass tH1F "h"] Nothing
  , Virtual void_ "SetMaximum" [double "maximum"] (Just "setMaximumTGraph")
  , Virtual void_ "SetMinimum" [double "minimum"] (Just "setMinimumTGraph")
  , Virtual void_ "Set" [int "n"] Nothing
  , Virtual void_ "SetPoint" [int "i", double "x", double "y"] Nothing
  -- Zero
  ]

tGraphAsymmErrors :: Class
tGraphAsymmErrors =
  histclass "TGraphAsymmErrors" [tGraph] (Protected [])
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh" ] Nothing
  ]

tGraphBentErrors :: Class
tGraphBentErrors =
  histclass "TGraphBentErrors" [tGraph] (Protected [])
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh", doublep "exld", doublep "exhd", doublep "eyld", doublep "eyhd"] Nothing
  ]

tGraphErrors :: Class
tGraphErrors =
  histclass "TGraphErrors" [tGraph] (Protected [])
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] Nothing
  ]


----------------
-- starting H --
----------------


tH1 :: Class
tH1 =
  histclass "TH1" [tNamed, tAttLine, tAttFill, tAttMarker] (Protected [])
  [ Virtual void_ "Add" [cppclass tH1 "h1", double "c1"] Nothing
  , Virtual void_ "AddBinContent" [int "bin", double "w"] Nothing
  , Virtual double_ "Chi2Test" [cppclass tH1 "h2", cstring "option", doublep "res"] Nothing
  , Virtual double_ "ComputeIntegral" [] Nothing
  , Virtual void_ "DirectoryAutoAdd" [cppclass tDirectory "dir"] Nothing

  , Virtual void_ "Divide" [cppclass tH1 "h1", cppclass tH1 "h2", double "c1", double "c2", cstring "option"] Nothing
  , Virtual self_ "DrawCopy" [cstring "option"] (Just "drawCopyTH1")
  , Virtual (cppclass_ tH1) "DrawNormalized" [cstring "option", double "norm"] Nothing
  , Virtual void_ "DrawPanel" [] (Just "drawPanelTH1")
  , Virtual int_ "BufferEmpty" [int "action"] Nothing
  , Virtual void_ "Eval" [cppclass tF1 "f1", cstring "option"] (Just "evalF")
  , Virtual (cppclass_ tH1) "FFT" [cppclass tH1 "h_output", cstring "option"] Nothing

  , Virtual int_  "Fill" [double "x"] (Just "fill1")
  , Virtual int_  "Fill" [double "x", double "w"] (Just "fill1w")
  , Virtual void_ "FillN" [int "ntimes", doublep "x", doublep "w", int "stride"] (Just "fillN1")
  , Virtual void_ "FillRandom" [cppclass tH1 "h", int "ntimes"] Nothing
  , Virtual int_ "FindBin" [double "x", double "y", double "z"] Nothing
  , Virtual int_ "FindFixBin" [double "x", double "y", double "z"] Nothing
  , Virtual int_ "FindFirstBinAbove" [double "threshold", int "axis"] Nothing
  , Virtual int_ "FindLastBinAbove" [double "threshold", int "axis"] Nothing
  -- Fit
  , Virtual void_ {- (cppclass_ tFitResultPtr) -} "Fit" [cppclass tF1 "f1", cstring "option", cstring "goption", double "xmin", double "xmax"] Nothing
  , Virtual void_ "FitPanel" [] (Just "FitPanelTH1")
  , NonVirtual self_ "GetAsymmetry" [cppclass tH1 "h2", double "c2", double "dc2"] Nothing
  , NonVirtual int_ "GetBufferLength" [] Nothing
  , NonVirtual int_ "GetBufferSize" [] Nothing
  -- GetBuffer
  , Static int_ "GetDefaultBufferSize" [] Nothing
  -- GetIntegral
  -- GetListOfFunctions
  , Virtual int_ "GetNdivisions" [cstring "axis"] (Just "getNdivisionA")
  , Virtual short_ "GetAxisColor" [cstring "axis"] (Just "getAxisColorA")
  , Virtual short_ "GetLabelColor" [cstring "axis"] (Just "getLabelColorA")
  , Virtual short_ "GetLabelFont" [cstring "axis"] (Just "getLabelFontA")
  , Virtual float_ "GetLabelOffset" [cstring "axis"] (Just "getLabelOffsetA")
  , Virtual float_ "GetLabelSize" [cstring "axis"] (Just "getLabelSizeA")
  , Virtual short_ "GetTitleFont" [cstring "axis"] (Just "getTitleFontA")
  , Virtual float_ "GetTitleOffset" [cstring "axis"] (Just "getTitleOffsetA")
  , Virtual float_ "GetTitleSize" [cstring "axis"] (Just "getTitleSizeA")
  , Virtual float_ "GetTickLength" [cstring "axis"] (Just "getTickLengthA")
  , Virtual float_ "GetBarOffset" [] Nothing
  , Virtual float_ "GetBarWidth" [] Nothing
  , Virtual int_ "GetContour" [doublep "levels"] Nothing
  , Virtual double_ "GetContourLevel" [int "level"] Nothing
  , Virtual double_ "GetContourLevelPad" [int "level"] Nothing
  , Virtual int_ "GetBin" [int "binx", int "biny", int "binz"] Nothing
  -- GetBinXYZ
  , Virtual double_ "GetBinCenter" [int "bin"] Nothing
  , Virtual double_ "GetBinContent" [int "binx"] (Just "GetBinContent1")
  , Virtual double_ "GetBinContent" [int "binx", int "biny"] (Just "GetBinContent2")
  , Virtual double_ "GetBinContent" [int "binx", int "biny", int "binz"] (Just "GetBinContent3")
  , Virtual double_ "GetBinError" [int "binx"] (Just "GetBinError1")
  , Virtual double_ "GetBinError" [int "binx", int "biny"] (Just "GetBinError2")
  , Virtual double_ "GetBinError" [int "binx", int "biny", int "binz"] (Just "GetBinError3")
  , Virtual double_ "GetBinLowEdge" [int "bin"] Nothing
  , Virtual double_ "GetBinWidth" [int "bin"] Nothing
  -- GetBinWidthContent
  , Virtual double_ "GetCellContent" [int "binx", int "biny"] Nothing
  , Virtual double_ "GetCellError" [int "binx", int "biny"] Nothing
  -- GetCenter
  , Static  bool_ "GetDefaultSumw2" [] Nothing
  , NonVirtual (cppclass_ tDirectory) "GetDirectory" [] Nothing
  , Virtual double_ "GetEntries" [] Nothing
  , Virtual double_ "GetEffectiveEntries" [] Nothing
  , Virtual (cppclass_ tF1) "GetFunction" [cstring "name"] Nothing
  , Virtual int_ "GetDimension" [] Nothing
  , Virtual double_ "GetKurtosis" [int "axis"] Nothing
  , Virtual void_ "GetLowEdge" [doublep "edge"] Nothing
  , Virtual double_ "GetMaximum" [double "maxval"] (Just "getMaximumTH1")
  , Virtual int_ "GetMaximumBin" [] Nothing
  , Virtual double_ "GetMaximumStored" [] Nothing
  , Virtual double_ "GetMinimum" [double "minval"] (Just "getMinimumTH1")
  , Virtual int_ "GetMinimumBin" [] Nothing
  , Virtual double_ "GetMinimumStored" [] Nothing
  , Virtual double_ "GetMean" [int "axis"] Nothing
  , Virtual double_ "GetMeanError" [int "axis"] Nothing
  , Virtual double_ "GetNbinsX" [] Nothing
  , Virtual double_ "GetNbinsY" [] Nothing
  , Virtual double_ "GetNbinsZ" [] Nothing
   -- GetObjectInfo
   -- GetOption
  -- , Virtual (cppclass_ "TVirtualHistPainter") "GetPainter" [cstring "option"]
  , Virtual int_ "GetQuantiles" [int "nprobSum", doublep "q", doublep "pbSum"] (Just "getQuantilesTH1")
  , Virtual double_ "GetRandom" [] Nothing
  , Virtual void_ "GetStats" [doublep "stats"] Nothing
  , Virtual double_ "GetSumOfWeights" [] Nothing
  , Virtual (cppclass_ tArrayD) "GetSumw2" [] Nothing
  , Virtual int_ "GetSumw2N" [] Nothing
  , Virtual double_ "GetRMS" [int "axis"] Nothing
  , Virtual double_ "GetRMSError" [int "axis"] Nothing
  , Virtual double_ "GetSkewness" [int "axis"] Nothing
  , NonVirtual (cppclass_ tAxis) "GetXaxis" [] Nothing
  , NonVirtual (cppclass_ tAxis) "GetYaxis" [] Nothing
  , NonVirtual (cppclass_ tAxis) "GetZaxis" [] Nothing
  , Virtual double_ "Integral" [int "binx1", int "binx2", cstring "option"] (Just "integral1")
  -- IntegralAndError
  , Virtual double_ "Interpolate" [double "x"] (Just "interpolate1")
  , Virtual double_ "Interpolate" [double "x", double "y"] (Just "interpolate2")
  , Virtual double_ "Interpolate" [double "x", double "y", double "z"] (Just "interpolate3")
  , NonVirtual bool_ "IsBinOverflow" [int "bin"] Nothing
  , NonVirtual bool_ "IsBinUnderflow" [int "bin"] Nothing
  , Virtual double_ "KolmogorovTest" [cppclass tH1 "h2", cstring "option"] Nothing
  , Virtual void_ "LabelsDeflate" [cstring "axis"] Nothing
  , Virtual void_ "LabelsInflate" [cstring "axis"] Nothing
  , Virtual void_ "LabelsOption" [cstring "option", cstring "axis"] Nothing
  , Virtual void_ "Multiply" [cppclass tF1 "h1", double "c1"] (Just "multiflyF")
  , Virtual void_ "Multiply" [cppclass tH1 "h1", cppclass tH1 "h2", double "c1", double "c2", cstring "option"] Nothing
  , Virtual void_ "PutStats" [doublep "stats"] Nothing
  , Virtual (cppclass_ tH1) "Rebin" [int "ngroup", cstring "newname", doublep "xbins"] Nothing
  , Virtual void_ "RebinAxis" [double "x", cppclass tAxis "axis"] Nothing
  , Virtual void_ "Rebuild" [cstring "option"] Nothing
  , Virtual void_ "RecursiveRemove" [cppclass tObject "obj"] Nothing
  , Virtual void_ "Reset" [cstring "option"] Nothing
  , Virtual void_ "ResetStats" [] Nothing
  , Virtual void_ "Scale" [double "c1", cstring "option"] Nothing
  , Virtual void_ "SetAxisColor" [short "color", cstring "axis"] (Just "setAxisColorA")
  , Virtual void_ "SetAxisRange" [double "xmin", double "xmax", cstring "axis"] Nothing
  , Virtual void_ "SetBarOffset" [float "offset"] Nothing
  , Virtual void_ "SetBarWidth" [float "width"] Nothing
  , Virtual void_ "SetBinContent" [int "bin", double "content"] (Just "setBinContent1")
  , Virtual void_ "SetBinContent" [int "binx", int "biny", double "content"] (Just "setBinContent2")
  , Virtual void_ "SetBinContent" [int "binx", int "biny", int "binz", double "content"] (Just "setBinContent3")
  , Virtual void_ "SetBinError" [int "bin", double "error"] (Just "setBinError1")
  , Virtual void_ "SetBinError" [int "binx", int "biny", double "error"] (Just "setBinError2")
  , Virtual void_ "SetBinError" [int "binx", int "biny", int "binz", double "error"] (Just "setBinError3")
  , Virtual void_ "SetBins" [int "nx", doublep "xBins"] (Just "setBins1")
  , Virtual void_ "SetBins" [int "nx", doublep "xBins", int "ny", doublep "yBins"] (Just "setBins2")
  , Virtual void_ "SetBins" [int "nx", doublep "xBins", int "ny", doublep "yBins", int "nz", doublep "zBins"] (Just "setBins3")
  , Virtual void_ "SetBinsLength" [int "bin"] Nothing
  , Virtual void_ "SetBuffer" [int "buffersize", cstring "option"] Nothing
  , Virtual void_ "SetCellContent" [int "binx", int "biny", double "content"] Nothing
  , Virtual void_ "SetContent" [doublep "content"] Nothing
  , Virtual void_ "SetContour" [int "nlevels", doublep "levels"] Nothing
  , Virtual void_ "SetContourLevel" [int "level", double "value"] Nothing
  , Static  void_ "SetDefaultBufferSize" [int "buffersize"] Nothing
  , Static  void_ "SetDefaultSumw2" [bool "sumw2"] Nothing
  -- SetDefaultSumw2
  , Virtual void_ "SetDirectory" [cppclass tDirectory "dir"] Nothing
  , Virtual void_ "SetEntries" [double "n"] Nothing
  , Virtual void_ "SetError" [doublep "error"] Nothing
  , Virtual void_ "SetLabelColor" [short "color", cstring "axis"] (Just "setLabelColorA")
  , Virtual void_ "SetLabelSize" [float "size", cstring "axis"] (Just "setLabelSizeA")
  , Virtual void_   "SetLabelFont"    [short "font", cstring "axis"] (Just "setLabelFontA")
  , Virtual void_   "SetLabelOffset"  [float "offset", cstring "axis"] (Just "setLabelOffsetA")
  , Virtual void_ "SetMaximum" [double "maximum"] Nothing
  , Virtual void_ "SetMinimum" [double "minimum"] Nothing
  , Virtual void_ "SetNormFactor" [double "factor"] Nothing
  , Virtual void_ "SetStats" [bool "stats"] Nothing
  , Virtual void_ "SetOption" [cstring "option"] Nothing
  , Virtual void_ "SetXTitle" [cstring "title"] Nothing
  , Virtual void_ "SetYTitle" [cstring "title"] Nothing
  , Virtual void_ "SetZTitle" [cstring "title"] Nothing
  , Virtual (cppclass_ tH1) "ShowBackground" [int "niter", cstring "option"] Nothing
  , Virtual int_  "ShowPeaks" [double "sigma", cstring "option", double "threshold" ] Nothing
  , Virtual void_ "Smooth" [int "ntimes", cstring "option"] Nothing
  , Static  void_ "SmoothArray" [int "NN", doublep "XX", int "ntimes"] Nothing
  , Static  void_ "StatOverflows" [bool "flag"] Nothing
  , Virtual void_ "Sumw2" [] Nothing
  , NonVirtual void_ "UseCurrentStyle" [] Nothing
  -- TransformHisto
  ]

tH1C :: Class
tH1C = histclass "TH1C" [tH1, tArrayC] (Protected [])
       []

tH1D :: Class
tH1D = histclass "TH1D" [tH1, tArrayD] (Protected [])
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"] Nothing
       ]

tH1F :: Class
tH1F = histclass "TH1F" [tH1, tArrayF] (Protected [])
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"] Nothing
       ]

tH1I :: Class
tH1I = histclass "TH1I" [tH1, tArrayI] (Protected [])
       []

tH1K :: Class
tH1K = histclass "TH1K" [tH1, tArrayF] (Protected [])
       []

tH1S :: Class
tH1S = histclass "TH1S" [tH1, tArrayS] (Protected [])
       []


tH2 :: Class
tH2 =
  histclass "TH2" [tH1] (Protected ["fill1"])
  [ Virtual int_ "Fill" [double "x", double "y"] (Just "fill2")
  , Virtual int_ "Fill" [double "x", double "y", double "w"] (Just "fill2w")
  , Virtual void_ "FillN" [int "ntimes", doublep "x",  doublep "y", doublep "w", int "stride"] (Just "fillN2")
  , Virtual void_ "FillRandom" [cppclass tH1 "h", int "ntimes"] (Just "fillRandom2")
  , Virtual int_  "FindFirstBinAbove" [double "threshold", int "axis"] (Just "findFirstBinAbove2")
  , Virtual int_  "FindLastBinAbove"  [double "threshold", int "axis"] (Just "findLastBinAbove2")
  , Virtual void_ "FitSlicesX" [cppclass tF1 "f1", int "firstybin", int "lastybin", int "cut", cstring "option", cppclass tObjArray "arr"] Nothing
  , Virtual void_ "FitSlicesY" [cppclass tF1 "f1", int "firstxbin", int "lastxbin", int "cut", cstring "option", cppclass tObjArray "arr"] Nothing
  -- GetBinWithContent2
  , Virtual double_ "GetCorrelationFactor" [int "axis1", int "axis2"] (Just "getCorrelationFactor2")
  , Virtual double_ "GetCovariance" [int "axis1", int "axis2"] (Just "getCovariance2")
  -- GetRandom2
  , Virtual double_ "Integral" [int "binx1", int "binx2", int "biny1", int "biny2", cstring "option"] (Just "integral2")
  , NonVirtual (cppclass_ tH1D) "ProjectionX" [cstring "name", int "firstybin", int "lastybin", cstring "option" ] Nothing
  , NonVirtual (cppclass_ tH1D) "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", cstring "option" ] Nothing
  , Virtual (cppclass_ tH2) "RebinX" [int "ngroup", cstring "newname"] (Just "rebinX2")
  , Virtual (cppclass_ tH2) "RebinY" [int "ngroup", cstring "newname"] (Just "rebinY2")
  , Virtual (cppclass_ tH2) "Rebin2D" [int "nxgroup", int "nygroup", cstring "newname"] Nothing
  , Virtual void_ "SetShowProjectionX" [int "nbins"] Nothing
  , Virtual void_ "SetShowProjectionY" [int "nbins"] Nothing
  ]

tH2C :: Class
tH2C = histclass "TH2C" [tH2, tArrayC] (Protected ["fill1"])
       []

tH2D :: Class
tH2D = histclass "TH2D" [tH2, tArrayD] (Protected ["fill1"])
       [ Constructor [ cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                     , int "nbinsy", double "ylow", double "yup"] Nothing
       ]

tH2F :: Class
tH2F = histclass "TH2F" [tH2, tArrayF] (Protected ["fill1"])
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                              ,int "nbinsy", double "ylow", double "yup"] Nothing
       ]

tH2I :: Class
tH2I = histclass "TH2I" [tH2, tArrayI] (Protected ["fill1"])
       []

tH2Poly :: Class
tH2Poly = histclass "TH2Poly" [tH2] (Protected ["fill1"])
          []

tH2S :: Class
tH2S = histclass "TH2S" [tH2, tArrayS] (Protected ["fill1"])
       []

tH3 :: Class
tH3 =
  histclass "TH3" [tH1, tAtt3D] (Protected ["fill1","fill1w"])
  [ Virtual int_ "Fill" [double "x", double "y", double "z"] (Just "fill3")
  , Virtual int_ "Fill" [double "x", double "y", double "z", double "w"] (Just "fill3w")
  , Virtual void_ "FitSlicesZ" [cppclass tF1 "f1", int "binminx", int "binmaxx", int "binminy", int "binmaxy", int "cut", cstring "option" ] Nothing
  -- GetBinWithContent3
  , Virtual double_ "GetCorrelationFactor" [int "axis1", int "axis2"] (Just "getCorrelationFactor3")
  , Virtual double_ "GetCovariance" [int "axis1", int "axis2"] (Just "getCovariance3")
  -- GetRandom3
  , NonVirtual (cppclass_ tH1D) "ProjectionX" [cstring "name", int "firstybin", int "lastybin", int "firstzbin", int "lastzbin", cstring "option" ] Nothing
  , NonVirtual (cppclass_ tH1D) "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", int "firstzbin", int "lastzbin", cstring "option" ] Nothing
  , NonVirtual (cppclass_ tH1D) "ProjectionZ" [cstring "name", int "firstxbin", int "lastxbin", int "firstybin", int "lastybin", cstring "option" ] Nothing
  , NonVirtual (cppclass_ tH1) "Project3D" [cstring "option"] Nothing
  -- Project3DProfile
  , Virtual (cppclass_ tH3) "RebinX" [int "ngroup", cstring "newname"] (Just "rebinX3")
  , Virtual (cppclass_ tH3) "RebinY" [int "ngroup", cstring "newname"] (Just "rebinY3")
  , Virtual (cppclass_ tH3) "RebinZ" [int "ngroup", cstring "newname"] (Just "rebinZ3")
  , Virtual (cppclass_ tH3) "Rebin3D" [int "nxgroup", int "nygroup", int "nzgroup", cstring "newname"] Nothing
  ]

tH3C :: Class
tH3C = histclass "TH3C" [tH3, tArrayC] (Protected ["fill1","fill1w"])
       []

tH3D :: Class
tH3D = histclass "TH3D" [tH3, tArrayD] (Protected ["fill1","fill1w"])
       []

tH3F :: Class
tH3F = histclass "TH3F" [tH3, tArrayF] (Protected ["fill1","fill1w"])
       []

tH3I :: Class
tH3I = histclass "TH3I" [tH3, tArrayI] (Protected ["fill1","fill1w"])
       []

tH3S :: Class
tH3S = histclass "TH3S" [tH3, tArrayS] (Protected ["fill1","fill1w"])
       []

tHStack :: Class
tHStack = histclass "THStack" [tNamed] (Protected [])
          [ Constructor [cstring "name",cstring "title"] Nothing
          ]

hist_classes :: [Class]
hist_classes =
  [ tAxis
  , tF1, tFitResult, tFitResultPtr -- , tFormula
  , tGraph, tGraphAsymmErrors, tGraphBentErrors, tGraphErrors
  , tH1, tH1C, tH1D, tH1F, tH1I, tH1K, tH1S, tH2, tH2C, tH2D, tH2F, tH2I, tH2Poly, tH2S, tH3, tH3C, tH3D, tH3F, tH3I, tH3S, tHStack ]

hist_topfunctions :: [TopLevelFunction]
hist_topfunctions = []

hist_headers :: [(ModuleUnit,ModuleUnitImports)]
hist_headers =
  [ modImports "TAxis"             ["ROOT"] ["TAxis.h"]
  , modImports "TF1"               ["ROOT"] ["TF1.h"]
  , modImports "TFitResult"        ["ROOT"] ["TFitResult.h"]
  , modImports "TFitResultPtr"     ["ROOT"] ["TFitResultPtr.h"]
  , modImports "TGraph"            ["ROOT"] ["TGraph.h"]
  , modImports "TGraphAsymmErrors" ["ROOT"] ["TGraphAsymmErrors.h"]
  , modImports "TGraphBentErrors"  ["ROOT"] ["TGraphBentErrors.h"]
  , modImports "TGraphErrors"      ["ROOT"] ["TGraphErrors.h"]
  , modImports "TH1"               ["ROOT"] ["TH1.h"]
  , modImports "TH1C"              ["ROOT"] ["TH1C.h"]
  , modImports "TH1D"              ["ROOT"] ["TH1D.h"]
  , modImports "TH1F"              ["ROOT"] ["TH1F.h"]
  , modImports "TH1I"              ["ROOT"] ["TH1I.h"]
  , modImports "TH1K"              ["ROOT"] ["TH1K.h"]
  , modImports "TH1S"              ["ROOT"] ["TH1S.h"]
  , modImports "TH2"               ["ROOT"] ["TH2.h"]
  , modImports "TH2C"              ["ROOT"] ["TH2C.h"]
  , modImports "TH2D"              ["ROOT"] ["TH2D.h"]
  , modImports "TH2F"              ["ROOT"] ["TH2F.h"]
  , modImports "TH2I"              ["ROOT"] ["TH2I.h"]
  , modImports "TH2Poly"           ["ROOT"] ["TH2Poly.h"]
  , modImports "TH2S"              ["ROOT"] ["TH2S.h"]
  , modImports "TH3"               ["ROOT"] ["TH3.h"]
  , modImports "TH3C"              ["ROOT"] ["TH3C.h"]
  , modImports "TH3D"              ["ROOT"] ["TH3D.h"]
  , modImports "TH3F"              ["ROOT"] ["TH3F.h"]
  , modImports "TH3I"              ["ROOT"] ["TH3I.h"]
  , modImports "TH3S"              ["ROOT"] ["TH3S.h"]
  , modImports "THStack"           ["ROOT"] ["THStack.h"]
  ]

hist_extraLib :: [String]
hist_extraLib = []

hist_extraDep :: [(String,[String])]
hist_extraDep = []
