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
-- import Bindings.Cxx.Generate.Type.CType
-- import Bindings.Cxx.Generate.Type.Method
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module
-- 
import HROOT.Data.Core.Class

histcabal = Cabal { cabal_pkgname = "HROOT-hist"
                  , cabal_cheaderprefix = "HROOTHist" 
                  , cabal_moduleprefix = "HROOT.Hist" } 

histclass = Class histcabal

tGraph :: Class
tGraph = 
  Class histcabal "TGraph" [tNamed {- , tAttLine, tAttFill, tAttMarker -}] mempty
  [ Constructor [int "n", doublep "x", doublep "y"] 
  -- , Virtual void_ "Apply" [cppclass "TF1" "f"] 
  -- , Virtual double_ "Chisquare" [cppclass "TF1" "f1"]
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
  -- , NonVirtual (cppclass_ "TF1") "GetFunction" [cstring "name"]
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
  -- , NonVirtual (cppclass_ "TAxis") "GetXaxis" []
  -- , NonVirtual (cppclass_ "TAxis") "GetYaxis" [] 
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
  -- , Virtual void_ "PaintStats" [cppclass "TF1" "fit"] 
  , Virtual int_ "RemovePoint" [int "ipoint"]
  , Virtual void_ "SetEditable" [bool "editable"] 
  , Virtual void_ "SetHistogram" [cppclass tH1F "h"] 
  , AliasVirtual void_ "SetMaximum" [double "maximum"] "setMaximumTGraph"
  , AliasVirtual void_ "SetMinimum" [double "minimum"] "setMinimumTGraph"
  , Virtual void_ "Set" [int "n"]
  , Virtual void_ "SetPoint" [int "i", double "x", double "y"] 
  -- Zero
  ]


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
  -- , AliasVirtual void_ "Eval" [cppclass "TF1" "f1", cstring "option"] "evalF"
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
  -- , Virtual (cppclass_ "TF1") "GetFunction" [cstring "name"]
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
  -- , AliasVirtual void_ "Multiply" [cppclass "TF1" "h1", double "c1"] "multiflyF"
  , Virtual void_ "Multiply" [cppclass tH1 "h1", cppclass tH1 "h2", double "c1", double "c2", cstring "option"] 
  , Virtual void_ "PutStats" [doublep "stats"]
  , Virtual (cppclass_ tH1) "Rebin" [int "ngroup", cstring "newname", doublep "xbins"]
  -- , Virtual void_ "RebinAxis" [double "x", cppclass "TAxis" "axis"]
  , Virtual void_ "Rebuild" [cstring "option"]
  -- , Virtual void_ "RecursiveRemove" [cppclass "TObject" "obj"]
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


tH1F :: Class
tH1F = Class histcabal "TH1F" [tH1, tArrayF] mempty
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"]
       ] 

tH2 :: Class 
tH2 = 
  histclass "TH2" [tH1] (Protected ["fill1"])
  [ AliasVirtual int_ "Fill" [double "x", double "y"] "fill2"
  , AliasVirtual int_ "Fill" [double "x", double "y", double "w"] "fill2w"
  , AliasVirtual void_ "FillN" [int "ntimes", doublep "x",  doublep "y", doublep "w", int "stride"] "fillN2"
  , AliasVirtual void_ "FillRandom" [cppclass tH1 "h", int "ntimes"] "fillRandom2"
  , AliasVirtual int_  "FindFirstBinAbove" [double "threshold", int "axis"] "findFirstBinAbove2"
  , AliasVirtual int_  "FindLastBinAbove"  [double "threshold", int "axis"] "findLastBinAbove2"
{-  , Virtual void_ "FitSlicesX" [cppclass "TF1" "f1", int "firstybin", int "lastybin", int "cut", cstring "option", cppclass "TObjArray" "arr"]
  , Virtual void_ "FitSlicesY" [cppclass "TF1" "f1", int "firstxbin", int "lastxbin", int "cut", cstring "option", cppclass "TObjArray" "arr"]
  -- GetBinWithContent2
  , AliasVirtual double_ "GetCorrelationFactor" [int "axis1", int "axis2"] "getCorrelationFactor2"
  , AliasVirtual double_ "GetCovariance" [int "axis1", int "axis2"] "getCovariance2"
  -- GetRandom2
  , AliasVirtual double_ "Integral" [int "binx1", int "binx2", int "biny1", int "biny2", cstring "option"] "integral2"
  , NonVirtual (cppclass_ "TH1D") "ProjectionX" [cstring "name", int "firstybin", int "lastybin", cstring "option" ]
  , NonVirtual (cppclass_ "TH1D") "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", cstring "option" ] 
  , AliasVirtual (cppclass_ "TH2") "RebinX" [int "ngroup", cstring "newname"] "rebinX2"
  , AliasVirtual (cppclass_ "TH2") "RebinY" [int "ngroup", cstring "newname"] "rebinY2"
  , Virtual (cppclass_ "TH2") "Rebin2D" [int "nxgroup", int "nygroup", cstring "newname"]
  , Virtual void_ "SetShowProjectionX" [int "nbins"]
  , Virtual void_ "SetShowProjectionY" [int "nbins"] -}
  ]

tH2F :: Class
tH2F = histclass "TH2F" [tH2, tArrayF] (Protected ["fill1"])
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                              ,int "nbinsy", double "ylow", double "yup"] 
       ]





hist_classes :: [Class]
hist_classes = 
  [ tGraph, tH1, tH1F, tH2, tH2F ] 

