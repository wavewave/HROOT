-- |
-- Module      : HROOT.Data.Core.ROOTsmall
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Hist.Class where

import Data.Monoid 
-- 
-- import FFICXX.Generate.Type.CType
-- import FFICXX.Generate.Type.Method
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
-- 
import HROOT.Data.Core.Class

histcabal = Cabal { cabal_pkgname = "HROOT-hist"
                  , cabal_cheaderprefix = "HROOTHist" 
                  , cabal_moduleprefix = "HROOT.Hist" } 

histclass = Class histcabal

----------------
-- starting A --
----------------

tAxis :: Class
tAxis = 
  histclass "TAxis" [tNamed, tAttAxis] mempty
  [ Constructor [int "nbins", double "xmin", double "xmax"] 
  , AliasVirtual int_ "FindBin" [double "x"] "findBinTAxis"
  , AliasVirtual int_ "FindFixBin" [double "x"] "findFixBinTAxis"
  , AliasVirtual double_ "GetBinCenter" [int "bin"]  "getBinCenterTAxis"
  , Virtual double_ "GetBinCenterLog" [int "bin"]  
  -- , Virtual double_ "GetBinLabel" [int "bin"]
  , Virtual double_ "GetBinUpEdge" [int "bin"]
  -- GetCenter
  , NonVirtual bool_ "GetCenterLabels" []
  , NonVirtual bool_ "GetCenterTitle" [] 
  -- GetLowEdge
  , Virtual void_ "SetTimeDisplay" [ int "value" ] 
  , Virtual void_ "SetTimeFormat" [ cstring "format" ] 
  , Virtual void_ "SetTimeOffset" [double "toffset", cstring "option"]
  ]

----------------
-- starting F --
----------------


tF1 :: Class
tF1 = 
  histclass "TF1" [tFormula, tAttLine, tAttFill, tAttMarker] mempty
  [ Constructor [cstring "name",cstring "formula",double "xmin",double "xmax"] 
  -- Browse
  , Virtual double_ "Derivative" [double "x", doublep "params", double "epsilon"] 
  , Virtual double_ "Derivative2" [double "x", doublep "params", double "epsilon"] 
  , Virtual double_ "Derivative3" [double "x", doublep "params", double "epsilon"]
  , Static  double_ "DerivativeError" []
  -- DerivativeError
  , AliasVirtual self_ "DrawCopy" [cstring "option"] "drawCopyTF1"
  , Virtual (cppclass_ tObject) "DrawDerivative" [cstring "option"]
  , Virtual (cppclass_ tObject) "DrawIntegral" [cstring "option"]
  , Virtual void_ "DrawF1" [cstring "formula", double "xmin", double "xmax", cstring "option"]
  , Virtual void_ "FixParameter" [int "ipar", double "value"] 
  , NonVirtual double_ "GetChisquare" [] 
  , NonVirtual (cppclass_ tH1)  "GetHistogram" [] 
  , AliasVirtual double_ "GetMaximum" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] "getMaximumTF1"
  , AliasVirtual double_ "GetMinimum" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] "getMinimumTF1"
  , Virtual double_ "GetMaximumX" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"]
  , Virtual double_ "GetMinimumX" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"]
  , Virtual int_ "GetNDF" [] 
  , Virtual int_ "GetNpx" []
  -- GetMethodCall
  , Virtual int_ "GetNumberFreeParameters" [] 
  , Virtual int_ "GetNumberFitPoints" []
  , NonVirtual (cppclass_ tObject) "GetParent" [] 
  , Virtual double_ "GetParError" [int "ipar"] 
  -- GetParErrors 
  -- GetParLiits
  , Virtual double_ "GetProb" [] 
  , AliasVirtual int_ "GetQuantiles" [int "nprobSum", doublep "q", doublep "probSum"] "getQuantilesTF1"
  , AliasVirtual double_ "GetRandom" [double "xmin", double "xmax"] "getRandomTF1"
  -- GetRange
  , Virtual double_ "GetSave" [doublep "x"] 
  , Virtual double_ "GetX" [double "y", double "xmin", double "xmax", double "epsilon", int "maxiter"] 
  , Virtual double_ "GetXmin" []
  , Virtual double_ "GetXmax" [] 
  , NonVirtual (cppclass_ tAxis) "GetXaxis" [] 
  , NonVirtual (cppclass_ tAxis) "GetYaxis" [] 
  , NonVirtual (cppclass_ tAxis) "GetZaxis" [] 
  , Virtual double_ "GradientPar" [int "ipar", doublep "x", double "eps"] 
  , Virtual void_ "InitArgs" [doublep "x", doublep "params"] 
  , Static  void_ "InitStandardFunctions" []
  , AliasVirtual double_ "Integral" [double "a", double "b", doublep "params", double "epsilon"] "IntegralTF1"
  , Virtual double_ "IntegralError" [double "a", double "b", doublep "params", doublep "covmat", double "epsilon"]
  , Virtual double_ "IntegralFast" [int "num", doublep "x", doublep "w", double "a", double "b", doublep "params", double "epsilon"] 
  -- IntegralMultiple
  , Virtual bool_ "IsInside" [doublep "x"]  
  , Virtual void_ "ReleaseParameter" [int "ipar"] 
  , Virtual void_ "SetChisquare" [double "chi2"] 
  -- SetFitResult
  , AliasVirtual void_ "SetMaximum" [double "maximum"] "setMaximumTF1"
  , AliasVirtual void_ "SetMinimum" [double "minimum"] "setMinimumTF1" 
  , Virtual void_ "SetNDF" [int "ndf"] 
  , Virtual void_ "SetNumberFitPoints" [int "npfits"] 
  , Virtual void_ "SetNpx" [int "npx"]
  , Virtual void_ "SetParError" [int "ipar", double "error"] 
  , Virtual void_ "SetParErrors" [doublep "errors"] 
  , Virtual void_ "SetParLimits" [int "ipar", double "parmin", double "parmax"] 
  , Virtual void_ "SetParent" [cppclass tObject "parent"] 
  , AliasVirtual void_ "SetRange" [double "xmin", double "xmax"] "setRange1"
  , AliasVirtual void_ "SetRange" [double "xmin", double "xmax", double "ymin", double "ymax"] "setRange2"
  , AliasVirtual void_ "SetRange" [double "xmin", double "xmax", double "ymin", double "ymax", double "zmin", double "zmax"] "setRange3"
  , Virtual void_ "SetSavedPoint" [int "point", double "value"] 

  , Static  (cppclass_ tF1) "GetCurrent" []
  , Static  void_ "AbsValue" [bool "reject"]
  , Static  void_ "RejectPoint" [bool "reject"]
  , Static  bool_ "RejectedPoint" []
  , Static  void_ "SetCurrent" [cppclass tF1 "f1"]
  -- RejectPoint 
  -- RejectedPoint 
  -- SetCurrent 
  , Virtual double_ "Moment" [double "n", double "a", double "b", doublep "params", double "epsilon"] 
  , Virtual double_ "CentralMoment" [double "n", double "a", double "b", doublep "params", double "epsilon"] 
  , Virtual double_ "Mean" [double "a", double "b", doublep "params", double "epsilon"]
  , Virtual double_ "Variance" [double "a", double "b", doublep "params", double "epsilon"] 
  , Static  void_ "CalcGaussLegendreSamplingPoints" [int "num", doublep "x", doublep "w", double "eps"]
  ]


tFormula :: Class
tFormula = histclass "TFormula" [tNamed] mempty
           [ Constructor [cstring "name", cstring "formula"] 
           , NonVirtual void_ "Optimize" [] 
           -- Analyze
           -- AnalyzeFunction 
           , Virtual int_ "Compile" [cstring "expression"]
           , Virtual void_ "Clear" [cstring "option"]
           -- DefinedString
           , Virtual double_ "DefinedValue" [int "code"]
           -- DefinedVariable
           , Virtual double_ "Eval" [double "x", double "y", double "z", double "t"]
           , Virtual double_ "EvalParOld" [doublep "x", doublep "params"]
           , Virtual double_ "EvalPar" [doublep "x", doublep "params"]
           -- GetLinearPart
           , Virtual int_ "GetNdim" [] 
           , Virtual int_ "GetNpar" [] 
           , Virtual int_ "GetNumber" [] 
           -- GetExpFormula
           , NonVirtual double_ "GetParameter"    [cstring "name" ] 
           -- GetParameters
           -- GetParName
           , Virtual int_   "GetParNumber" [cstring "name"]
           , Virtual bool_  "IsLinear" [] 
           , Virtual bool_  "IsNormalized" [] 
           -- ProcessLinear
           , Virtual void_  "SetNumber" [int "number"]
           , Virtual void_  "SetParameter" [cstring "name", double "parvalue"]
           , Virtual void_  "SetParameters" [doublep "params"]
           , Virtual void_  "SetParName"  [int "ipar", cstring "name"] 
           , Virtual void_  "SetParNames" [cstring "name0", cstring "name1", cstring "name2"
                                          ,cstring "name3", cstring "name4", cstring "name5"
                                          ,cstring "name6", cstring "name7", cstring "name8"
                                          ,cstring "name9", cstring "name10" ]
           , Virtual void_  "Update" [] 
           -- SetMaxima
           ]



----------------
-- starting G --
----------------


tGraph :: Class
tGraph = 
  histclass "TGraph" [tNamed, tAttLine, tAttFill, tAttMarker] mempty
  [ Constructor [int "n", doublep "x", doublep "y"] 
  , Virtual void_ "Apply" [cppclass tF1 "f"] 
  , Virtual double_ "Chisquare" [cppclass tF1 "f1"]
  -- CompareArg
  -- CompareX
  -- CompareY 
  -- CompareRadius
  -- ComputeRange
  , Virtual void_ "DrawGraph" [int "n", doublep "x", doublep "y", cstring "option"] 
  , AliasVirtual void_ "DrawPanel" [] "drawPanelTGraph"
  -- Eval 
  , Virtual void_ "Expand" [int "newsize", int "step"]
  -- Fit
  , AliasVirtual void_ "FitPanel" [] "FitPanelTGraph"
  , NonVirtual bool_ "GetEditable" [] 
  , NonVirtual (cppclass_ tF1) "GetFunction" [cstring "name"]
  , NonVirtual (cppclass_ tH1F) "GetHistogram" [] 
  -- , NonVirtual (cppclass_ "TList") "GetListOfFunctions" [] 
  , AliasVirtual double_ "GetCorrelationFactor" [] "getCorrelationFactorTGraph"
  , AliasVirtual double_ "GetCovariance" [] "getCovarianceTGraph"
  , AliasVirtual double_ "GetMean" [int "axis"] "getMeanTGraph"
  , AliasVirtual double_ "GetRMS" [int "axis"] "getRMSTGraph"
  , NonVirtual int_ "GetMaxSize" [] 
  , NonVirtual int_ "GetN" []
  , Virtual double_ "GetErrorX" [int "bin"] 
  , Virtual double_ "GetErrorY" [int "bin"]
  , Virtual double_ "GetErrorXhigh" [int "bin"]
  , Virtual double_ "GetErrorXlow" [int "bin"]
  , Virtual double_ "GetErrorYhigh" [int "bin"]
  , Virtual double_ "GetErrorYlow" [int "bin"]
  -- GetX
  -- GetY
  -- GetEX
  -- GetEY
  -- omit.. 
  , NonVirtual double_ "GetMaximum" [] 
  , NonVirtual double_ "GetMinimum" []
  , NonVirtual (cppclass_ tAxis) "GetXaxis" []
  , NonVirtual (cppclass_ tAxis) "GetYaxis" [] 
  -- GetPoint
  , Virtual void_ "InitExpo" [double "xmin", double "xmax"]
  , Virtual void_ "InitGaus" [double "xmin", double "xmax"]
  , Virtual void_ "InitPolynom" [double "xmin", double "xmax"]
  , Virtual int_ "InsertPoint" [] 
  , AliasVirtual double_ "Integral" [int "first", int "last"] "integralTGraph"
  , Virtual bool_ "IsEditable" [] 
  , AliasVirtual int_ "IsInside" [double "x", double "y"] "isInsideTGraph"
  , Virtual void_ "LeastSquareFit" [int "m", doublep "a", double "xmin", double "xmax"] 
  -- LeastSquareLinearFit
  , NonVirtual void_ "PaintGraph" [int "npoints", doublep "x", doublep "y", cstring "chopt"] 
  , NonVirtual void_ "PaintGrapHist" [int "npoints", doublep "x", doublep "y", cstring "chopt"] 
  , Virtual void_ "PaintStats" [cppclass tF1 "fit"] 
  , Virtual int_ "RemovePoint" [int "ipoint"]
  , Virtual void_ "SetEditable" [bool "editable"] 
  , Virtual void_ "SetHistogram" [cppclass tH1F "h"] 
  , AliasVirtual void_ "SetMaximum" [double "maximum"] "setMaximumTGraph"
  , AliasVirtual void_ "SetMinimum" [double "minimum"] "setMinimumTGraph"
  , Virtual void_ "Set" [int "n"]
  , Virtual void_ "SetPoint" [int "i", double "x", double "y"] 
  -- Zero
  ]

tGraphAsymmErrors :: Class
tGraphAsymmErrors = 
  histclass "TGraphAsymmErrors" [tGraph] mempty
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh" ]  
  ]

tGraphBentErrors :: Class
tGraphBentErrors = 
  histclass "TGraphBentErrors" [tGraph] mempty
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh", doublep "exld", doublep "exhd", doublep "eyld", doublep "eyhd"] 
  ]

tGraphErrors :: Class
tGraphErrors = 
  histclass "TGraphErrors" [tGraph] mempty
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] 
  ]


----------------
-- starting H --
----------------


tH1 :: Class
tH1 = 
  histclass "TH1" [tObject, tAttLine, tAttFill, tAttMarker] mempty 
  [ Virtual void_ "Add" [cppclass tH1 "h1", double "c1"]  
  , Virtual void_ "AddBinContent" [int "bin", double "w"] 
  , Virtual double_ "Chi2Test" [cppclass tH1 "h2", cstring "option", doublep "res"] 
  , Virtual double_ "ComputeIntegral" []
  , Virtual void_ "DirectoryAutoAdd" [cppclass tDirectory "dir"]

  , Virtual void_ "Divide" [cppclass tH1 "h1", cppclass tH1 "h2", double "c1", double "c2", cstring "option"]
  , AliasVirtual self_ "DrawCopy" [cstring "option"] "drawCopyTH1"
  , Virtual (cppclass_ tH1) "DrawNormalized" [cstring "option", double "norm"]
  , AliasVirtual void_ "DrawPanel" [] "drawPanelTH1"
  , Virtual int_ "BufferEmpty" [int "action"]
  , AliasVirtual void_ "Eval" [cppclass tF1 "f1", cstring "option"] "evalF"
  , Virtual (cppclass_ tH1) "FFT" [cppclass tH1 "h_output", cstring "option"] 

  , AliasVirtual int_  "Fill" [double "x"] "fill1"
  , AliasVirtual int_  "Fill" [double "x", double "w"] "fill1w"
  , AliasVirtual void_ "FillN" [int "ntimes", doublep "x", doublep "w", int "stride"] "fillN1"
  , Virtual void_ "FillRandom" [cppclass tH1 "h", int "ntimes"] 
  , Virtual int_ "FindBin" [double "x", double "y", double "z"] 
  , Virtual int_ "FindFixBin" [double "x", double "y", double "z"]
  , Virtual int_ "FindFirstBinAbove" [double "threshold", int "axis"] 
  , Virtual int_ "FindLastBinAbove" [double "threshold", int "axis"]  
  -- Fit
  , AliasVirtual void_ "FitPanel" [] "FitPanelTH1"
  , NonVirtual self_ "GetAsymmetry" [cppclass tH1 "h2", double "c2", double "dc2"]
  , NonVirtual int_ "GetBufferLength" [] 
  , NonVirtual int_ "GetBufferSize" [] 
  -- GetBuffer
  , Static int_ "GetDefaultBufferSize" []
  -- GetIntegral
  -- GetListOfFunctions
  , AliasVirtual int_ "GetNdivisions" [cstring "axis"] "getNdivisionA"
  , AliasVirtual short_ "GetAxisColor" [cstring "axis"] "getAxisColorA"
  , AliasVirtual short_ "GetLabelColor" [cstring "axis"] "getLabelColorA"
  , AliasVirtual short_ "GetLabelFont" [cstring "axis"] "getLabelFontA"
  , AliasVirtual float_ "GetLabelOffset" [cstring "axis"] "getLabelOffsetA"
  , AliasVirtual float_ "GetLabelSize" [cstring "axis"] "getLabelSizeA"
  , AliasVirtual short_ "GetTitleFont" [cstring "axis"] "getTitleFontA"
  , AliasVirtual float_ "GetTitleOffset" [cstring "axis"] "getTitleOffsetA"
  , AliasVirtual float_ "GetTitleSize" [cstring "axis"] "getTitleSizeA"
  , AliasVirtual float_ "GetTickLength" [cstring "axis"] "getTickLengthA"
  , Virtual float_ "GetBarOffset" []
  , Virtual float_ "GetBarWidth" [] 
  , Virtual int_ "GetContour" [doublep "levels"] 
  , Virtual double_ "GetContourLevel" [int "level"] 
  , Virtual double_ "GetContourLevelPad" [int "level"] 
  , Virtual int_ "GetBin" [int "binx", int "biny", int "binz"]
  -- GetBinXYZ
  , Virtual double_ "GetBinCenter" [int "bin"]
  , AliasVirtual double_ "GetBinContent" [int "binx"] "GetBinContent1"
  , AliasVirtual double_ "GetBinContent" [int "binx", int "biny"] "GetBinContent2"
  , AliasVirtual double_ "GetBinContent" [int "binx", int "biny", int "binz"] "GetBinContent3" 
  , AliasVirtual double_ "GetBinError" [int "binx"] "GetBinError1"
  , AliasVirtual double_ "GetBinError" [int "binx", int "biny"] "GetBinError2"
  , AliasVirtual double_ "GetBinError" [int "binx", int "biny", int "binz"] "GetBinError3" 
  , Virtual double_ "GetBinLowEdge" [int "bin"] 
  , Virtual double_ "GetBinWidth" [int "bin"]
  -- GetBinWidthContent
  , Virtual double_ "GetCellContent" [int "binx", int "biny"] 
  , Virtual double_ "GetCellError" [int "binx", int "biny"]
  -- GetCenter
  , Static  bool_ "GetDefaultSumw2" []
  , NonVirtual (cppclass_ tDirectory) "GetDirectory" [] 
  , Virtual double_ "GetEntries" []
  , Virtual double_ "GetEffectiveEntries" [] 
  , Virtual (cppclass_ tF1) "GetFunction" [cstring "name"]
  , Virtual int_ "GetDimension" []
  , Virtual double_ "GetKurtosis" [int "axis"]
  , Virtual void_ "GetLowEdge" [doublep "edge"] 
  , AliasVirtual double_ "GetMaximum" [double "maxval"] "getMaximumTH1"
  , Virtual int_ "GetMaximumBin" []
  , Virtual double_ "GetMaximumStored" []
  , AliasVirtual double_ "GetMinimum" [double "minval"] "getMinimumTH1"
  , Virtual int_ "GetMinimumBin" []
  , Virtual double_ "GetMinimumStored" [] 
  , Virtual double_ "GetMean" [int "axis"]
  , Virtual double_ "GetMeanError" [int "axis"]
  , Virtual double_ "GetNbinsX" [] 
  , Virtual double_ "GetNbinsY" [] 
  , Virtual double_ "GetNbinsZ" [] 
   -- GetObjectInfo
   -- GetOption
  -- , Virtual (cppclass_ "TVirtualHistPainter") "GetPainter" [cstring "option"]
  , AliasVirtual int_ "GetQuantiles" [int "nprobSum", doublep "q", doublep "pbSum"] "getQuantilesTH1"
  , Virtual double_ "GetRandom" []
  , Virtual void_ "GetStats" [doublep "stats"]
  , Virtual double_ "GetSumOfWeights" [] 
  , Virtual (cppclass_ tArrayD) "GetSumw2" []
  , Virtual int_ "GetSumw2N" [] 
  , Virtual double_ "GetRMS" [int "axis"]
  , Virtual double_ "GetRMSError" [int "axis"] 
  , Virtual double_ "GetSkewness" [int "axis"]
  {- , NonVirtual (cppclass_ "TAxis") "GetXaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetYaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetZaxis" [] -}
  , AliasVirtual double_ "Integral" [int "binx1", int "binx2", cstring "option"] "integral1"
  -- IntegralAndError
  , AliasVirtual double_ "Interpolate" [double "x"] "interpolate1"
  , AliasVirtual double_ "Interpolate" [double "x", double "y"] "interpolate2"
  , AliasVirtual double_ "Interpolate" [double "x", double "y", double "z"] "interpolate3"
  , NonVirtual bool_ "IsBinOverflow" [int "bin"]
  , NonVirtual bool_ "IsBinUnderflow" [int "bin"]
  , Virtual double_ "KolmogorovTest" [cppclass tH1 "h2", cstring "option"]
  , Virtual void_ "LabelsDeflate" [cstring "axis"]
  , Virtual void_ "LabelsInflate" [cstring "axis"]
  , Virtual void_ "LabelsOption" [cstring "option", cstring "axis"]
  , AliasVirtual void_ "Multiply" [cppclass tF1 "h1", double "c1"] "multiflyF"
  , Virtual void_ "Multiply" [cppclass tH1 "h1", cppclass tH1 "h2", double "c1", double "c2", cstring "option"] 
  , Virtual void_ "PutStats" [doublep "stats"]
  , Virtual (cppclass_ tH1) "Rebin" [int "ngroup", cstring "newname", doublep "xbins"]
  , Virtual void_ "RebinAxis" [double "x", cppclass tAxis "axis"]
  , Virtual void_ "Rebuild" [cstring "option"]
  , Virtual void_ "RecursiveRemove" [cppclass tObject "obj"]
  , Virtual void_ "Reset" [cstring "option"]
  , Virtual void_ "ResetStats" [] 
  , Virtual void_ "Scale" [double "c1", cstring "option"]
  , AliasVirtual void_ "SetAxisColor" [short "color", cstring "axis"] "setAxisColorA"
  , Virtual void_ "SetAxisRange" [double "xmin", double "xmax", cstring "axis"]
  , Virtual void_ "SetBarOffset" [float "offset"]
  , Virtual void_ "SetBarWidth" [float "width"]
  , AliasVirtual void_ "SetBinContent" [int "bin", double "content"] "setBinContent1"
  , AliasVirtual void_ "SetBinContent" [int "binx", int "biny", double "content"] "setBinContent2"
  , AliasVirtual void_ "SetBinContent" [int "binx", int "biny", int "binz", double "content"] "setBinContent3"
  , AliasVirtual void_ "SetBinError" [int "bin", double "error"] "setBinError1"
  , AliasVirtual void_ "SetBinError" [int "binx", int "biny", double "error"] "setBinError2"
  , AliasVirtual void_ "SetBinError" [int "binx", int "biny", int "binz", double "error"] "setBinError3"
  , AliasVirtual void_ "SetBins" [int "nx", doublep "xBins"] "setBins1"
  , AliasVirtual void_ "SetBins" [int "nx", doublep "xBins", int "ny", doublep "yBins"] "setBins2"
  , AliasVirtual void_ "SetBins" [int "nx", doublep "xBins", int "ny", doublep "yBins", int "nz", doublep "zBins"] "setBins3"
  , Virtual void_ "SetBinsLength" [int "bin"]
  , Virtual void_ "SetBuffer" [int "buffersize", cstring "option"]
  , Virtual void_ "SetCellContent" [int "binx", int "biny", double "content"]
  , Virtual void_ "SetContent" [doublep "content"] 
  , Virtual void_ "SetContour" [int "nlevels", doublep "levels"] 
  , Virtual void_ "SetContourLevel" [int "level", double "value"]
  , Static  void_ "SetDefaultBufferSize" [int "buffersize"]
  , Static  void_ "SetDefaultSumw2" [bool "sumw2"]
  -- SetDefaultSumw2
  , Virtual void_ "SetDirectory" [cppclass tDirectory "dir"]
  , Virtual void_ "SetEntries" [double "n"]
  , Virtual void_ "SetError" [doublep "error"]
  , AliasVirtual void_ "SetLabelColor" [short "color", cstring "axis"] "setLabelColorA"
  , AliasVirtual void_ "SetLabelSize" [float "size", cstring "axis"] "setLabelSizeA"
  , AliasVirtual void_   "SetLabelFont"    [short "font", cstring "axis"] "setLabelFontA"
  , AliasVirtual void_   "SetLabelOffset"  [float "offset", cstring "axis"] "setLabelOffsetA"
  , Virtual void_ "SetMaximum" [double "maximum"]
  , Virtual void_ "SetMinimum" [double "minimum"]
  , Virtual void_ "SetNormFactor" [double "factor"] 
  , Virtual void_ "SetStats" [bool "stats"] 
  , Virtual void_ "SetOption" [cstring "option"] 
  , Virtual void_ "SetXTitle" [cstring "title"] 
  , Virtual void_ "SetYTitle" [cstring "title"]
  , Virtual void_ "SetZTitle" [cstring "title"]
  , Virtual (cppclass_ tH1) "ShowBackground" [int "niter", cstring "option"]
  , Virtual int_  "ShowPeaks" [double "sigma", cstring "option", double "threshold" ]
  , Virtual void_ "Smooth" [int "ntimes", cstring "option"] 
  , Static  void_ "SmoothArray" [int "NN", doublep "XX", int "ntimes"]
  , Static  void_ "StatOverflows" [bool "flag"]
  , Virtual void_ "Sumw2" [] 
  , NonVirtual void_ "UseCurrentStyle" [] 
  -- TransformHisto
  ] 

tH1C :: Class 
tH1C = histclass "TH1C" [tH1, tArrayC] mempty 
       []

tH1D :: Class
tH1D = histclass "TH1D" [tH1, tArrayD] mempty 
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"]
       ]

tH1F :: Class
tH1F = histclass "TH1F" [tH1, tArrayF] mempty
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"]
       ] 

tH1I :: Class 
tH1I = histclass "TH1I" [tH1, tArrayI] mempty
       []

tH1K :: Class
tH1K = histclass "TH1K" [tH1, tArrayF] mempty
       []

tH1S :: Class
tH1S = histclass "TH1S" [tH1, tArrayS] mempty 
       []


tH2 :: Class 
tH2 = 
  histclass "TH2" [tH1] (Protected ["fill1"])
  [ AliasVirtual int_ "Fill" [double "x", double "y"] "fill2"
  , AliasVirtual int_ "Fill" [double "x", double "y", double "w"] "fill2w"
  , AliasVirtual void_ "FillN" [int "ntimes", doublep "x",  doublep "y", doublep "w", int "stride"] "fillN2"
  , AliasVirtual void_ "FillRandom" [cppclass tH1 "h", int "ntimes"] "fillRandom2"
  , AliasVirtual int_  "FindFirstBinAbove" [double "threshold", int "axis"] "findFirstBinAbove2"
  , AliasVirtual int_  "FindLastBinAbove"  [double "threshold", int "axis"] "findLastBinAbove2"
  , Virtual void_ "FitSlicesX" [cppclass tF1 "f1", int "firstybin", int "lastybin", int "cut", cstring "option", cppclass tObjArray "arr"]
  , Virtual void_ "FitSlicesY" [cppclass tF1 "f1", int "firstxbin", int "lastxbin", int "cut", cstring "option", cppclass tObjArray "arr"]
  -- GetBinWithContent2
  , AliasVirtual double_ "GetCorrelationFactor" [int "axis1", int "axis2"] "getCorrelationFactor2"
  , AliasVirtual double_ "GetCovariance" [int "axis1", int "axis2"] "getCovariance2"
  -- GetRandom2
  , AliasVirtual double_ "Integral" [int "binx1", int "binx2", int "biny1", int "biny2", cstring "option"] "integral2"
  , NonVirtual (cppclass_ tH1D) "ProjectionX" [cstring "name", int "firstybin", int "lastybin", cstring "option" ]
  , NonVirtual (cppclass_ tH1D) "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", cstring "option" ] 
  , AliasVirtual (cppclass_ tH2) "RebinX" [int "ngroup", cstring "newname"] "rebinX2"
  , AliasVirtual (cppclass_ tH2) "RebinY" [int "ngroup", cstring "newname"] "rebinY2"
  , Virtual (cppclass_ tH2) "Rebin2D" [int "nxgroup", int "nygroup", cstring "newname"]
  , Virtual void_ "SetShowProjectionX" [int "nbins"]
  , Virtual void_ "SetShowProjectionY" [int "nbins"] 
  ]

tH2C :: Class
tH2C = histclass "TH2C" [tH2, tArrayC] (Protected ["fill1"])
       []

tH2D :: Class 
tH2D = histclass "TH2D" [tH2, tArrayD] (Protected ["fill1"])
       [ Constructor [ cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                     , int "nbinsy", double "ylow", double "yup"]
       ]

tH2F :: Class
tH2F = histclass "TH2F" [tH2, tArrayF] (Protected ["fill1"])
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                              ,int "nbinsy", double "ylow", double "yup"] 
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
  [ AliasVirtual int_ "Fill" [double "x", double "y", double "z"] "fill3"
  , AliasVirtual int_ "Fill" [double "x", double "y", double "z", double "w"] "fill3w"
  , Virtual void_ "FitSlicesZ" [cppclass tF1 "f1", int "binminx", int "binmaxx", int "binminy", int "binmaxy", int "cut", cstring "option" ] 
  -- GetBinWithContent3
  , AliasVirtual double_ "GetCorrelationFactor" [int "axis1", int "axis2"] "getCorrelationFactor3"
  , AliasVirtual double_ "GetCovariance" [int "axis1", int "axis2"] "getCovariance3"
  -- GetRandom3
  , NonVirtual (cppclass_ tH1D) "ProjectionX" [cstring "name", int "firstybin", int "lastybin", int "firstzbin", int "lastzbin", cstring "option" ]
  , NonVirtual (cppclass_ tH1D) "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", int "firstzbin", int "lastzbin", cstring "option" ]
  , NonVirtual (cppclass_ tH1D) "ProjectionZ" [cstring "name", int "firstxbin", int "lastxbin", int "firstybin", int "lastybin", cstring "option" ] 
  , NonVirtual (cppclass_ tH1) "Project3D" [cstring "option"]
  -- Project3DProfile
  , AliasVirtual (cppclass_ tH3) "RebinX" [int "ngroup", cstring "newname"] "rebinX3"
  , AliasVirtual (cppclass_ tH3) "RebinY" [int "ngroup", cstring "newname"] "rebinY3"
  , AliasVirtual (cppclass_ tH3) "RebinZ" [int "ngroup", cstring "newname"] "rebinZ3"
  , Virtual (cppclass_ tH3) "Rebin3D" [int "nxgroup", int "nygroup", int "nzgroup", cstring "newname"]
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
tHStack = histclass "THStack" [tNamed] mempty 
          [ Constructor [cstring "name",cstring "title"]  
          ] 




hist_classes :: [Class]
hist_classes = 
  [ tAxis
  , tF1, tFormula
  , tGraph, tGraphAsymmErrors, tGraphBentErrors, tGraphErrors
  , tH1, tH1C, tH1D, tH1F, tH1I, tH1K, tH1S, tH2, tH2C, tH2D, tH2F, tH2I, tH2Poly, tH2S, tH3, tH3C, tH3D, tH3F, tH3I, tH3S, tHStack ] 

