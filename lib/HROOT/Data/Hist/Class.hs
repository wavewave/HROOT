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
  Class histcabal "TGraph" [tNamed {- , tAttLine, tAttFill, tAttMarker -}] 
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
  Class histcabal  "TH1" [tObject] 
  [ AliasVirtual int_ "Fill" [double "x"] "fill1"
  ] 

tH1F :: Class
tH1F = Class histcabal "TH1F" [tH1, tArrayF] 
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"]
       ] 

tH2 :: Class 
tH2 = 
  histclass "TH2" [tH1] 
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
tH2F = histclass "TH2F" [tH2, tArrayF] 
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                              ,int "nbinsy", double "ylow", double "yup"] 
       ]





hist_classes :: [Class]
hist_classes = 
  [ tGraph, tH1, tH1F, tH2, tH2F ] 

