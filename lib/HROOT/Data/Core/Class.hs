-- |
-- Module      : HROOT.Data.Core.Class
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Core.Class where

import Data.Monoid
-- 
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module


corecabal = Cabal { cabal_pkgname = "HROOT-core"
                  , cabal_cheaderprefix = "HROOTCore" 
                  , cabal_moduleprefix = "HROOT.Core" 
                  } 

coreclass n ps ann fs = Class corecabal n ps ann Nothing fs 

{-
moduleInterface :: Module
moduleInterface = Module { module_name = "HROOT.Class.Interface"
                         , module_exports = [ "IDeletable" 
                                            , "TObject"
                                            , "ITObject"
                                            , "ITNamed" 
                                            , "TNamed" ]
                         }  
-}

deletable :: Class 
deletable = AbstractClass corecabal "Deletable" [] mempty Nothing
          [ Destructor Nothing ]


----------------
-- starting A --
----------------


tApplication :: Class
tApplication = coreclass "TApplication" [tObject, tQObject] mempty
               [ Constructor    [ cstring "appClassName", intp "argc", charpp "argv"  ] Nothing
               , Virtual void_ "Run"    [ bool "retrn"]  Nothing
               ]

tArray :: Class
tArray = coreclass "TArray" [deletable] mempty
         []

tArrayC :: Class 
tArrayC = coreclass "TArrayC" [tArray] mempty
          []

tArrayD :: Class
tArrayD = coreclass "TArrayD" [tArray] mempty
          []

tArrayF :: Class 
tArrayF = coreclass "TArrayF" [tArray] mempty
          []

tArrayI :: Class
tArrayI = coreclass "TArrayI" [tArray] mempty
          []

tArrayL :: Class
tArrayL = coreclass "TArrayL" [tArray] mempty
          []

tArrayL64 :: Class
tArrayL64 = coreclass "TArrayL64" [tArray] mempty
            []

tArrayS :: Class
tArrayS = coreclass "TArrayS" [tArray] mempty
          []

tAtt3D :: Class
tAtt3D = coreclass "TAtt3D" [deletable] mempty
         []

tAttAxis :: Class
tAttAxis = 
  coreclass "TAttAxis" [deletable] mempty 
  [ Constructor [] Nothing
  , Virtual int_ "GetNdivisions" [] Nothing
  , Virtual short_ "GetAxisColor" [] Nothing
  , Virtual short_ "GetLabelColor" [] Nothing
  , Virtual short_ "GetLabelFont" [] Nothing
  , Virtual float_ "GetLabelOffset" [] Nothing
  , Virtual float_ "GetLabelSize" [] Nothing
  , Virtual float_ "GetTitleOffset" [] Nothing
  , Virtual float_ "GetTitleSize" [] Nothing
  , Virtual float_ "GetTickLength" [] Nothing
  , Virtual short_ "GetTitleFont" [] Nothing
  -- omit..
  , Virtual void_   "SetNdivisions"   [int "n", bool "optim" ] Nothing
  , Virtual void_   "SetAxisColor"    [short  "color"] Nothing
  , Virtual void_   "SetLabelColor"   [short  "color" ] Nothing 
  , Virtual void_   "SetLabelFont"    [short "font"] Nothing
  , Virtual void_   "SetLabelOffset"  [float "offset"] Nothing
  , Virtual void_   "SetLabelSize"    [float  "size"  ] Nothing
  , Virtual void_   "SetTickLength"   [float  "length" ] Nothing
  , Virtual void_   "SetTitleOffset"  [float  "offset" ] Nothing
  , Virtual void_   "SetTitleSize"    [float  "size"] Nothing
  , Virtual void_   "SetTitleColor"   [short  "color"] Nothing
  , Virtual void_   "SetTitleFont"    [short  "font"] Nothing

  ] 


tAttBBox :: Class 
tAttBBox = coreclass "TAttBBox" [deletable] mempty 
           []

tAttCanvas :: Class
tAttCanvas = coreclass "TAttCanvas" [deletable] mempty
             [ Constructor []  Nothing
             ]

tAttFill :: Class
tAttFill = coreclass "TAttFill" [deletable] mempty
           [ Constructor [short "fcolor", short "fstyle"] Nothing
           , Virtual void_   "SetFillColor"    [int "color" ] Nothing
           , Virtual void_   "SetFillStyle"    [int "style" ] Nothing
           ]

tAttLine :: Class
tAttLine = 
  coreclass "TAttLine" [deletable] mempty
  [ Constructor [short "lcolor", short "lstyle", short "lwidth"] Nothing
  , NonVirtual int_ "DistancetoLine" [int "px", int "py", double "xp1", double "yp1", double "xp2", double "yp2"] Nothing
  , Virtual short_ "GetLineColor" [] Nothing
  , Virtual short_ "GetLineStyle" [] Nothing
  , Virtual short_ "GetLineWidth" [] Nothing
  --   , Virtual void_ "Modify" [] 
  , Virtual void_ "ResetAttLine" [cstring "option"] Nothing
  -- SaveLineAttributes 
  , Virtual void_ "SetLineAttributes" [] Nothing
  , Virtual void_ "SetLineColor" [short "lcolor" ] Nothing 
  , Virtual void_ "SetLineStyle" [short "lstyle" ] Nothing
  , Virtual void_ "SetLineWidth" [short "lwidth" ] Nothing
  ]


tAttMarker :: Class
tAttMarker = 
  coreclass "TAttMarker" [deletable] mempty
  [ Constructor [short "color", short "style", short "msize"] Nothing
  , Virtual short_ "GetMarkerColor" [] Nothing
  , Virtual short_ "GetMarkerStyle" [] Nothing
  , Virtual float_ "GetMarkerSize" [] Nothing
  -- Modify
  , Virtual void_ "ResetAttMarker" [cstring "option"] Nothing
  , Virtual void_ "SetMarkerAttributes" [] Nothing
  , Virtual void_ "SetMarkerColor" [short "tcolor"] Nothing
  , Virtual void_ "SetMarkerStyle" [short "mstyle"] Nothing
  , Virtual void_ "SetMarkerSize" [short "msize"] Nothing
  ]  

tAttPad :: Class
tAttPad = 
  coreclass "TAttPad" [deletable] mempty
  [ Constructor [] Nothing
  , NonVirtual float_ "GetBottomMargin" [] Nothing
  , NonVirtual float_ "GetLeftMargin" [] Nothing
  , NonVirtual float_ "GetRightMargin" [] Nothing
  , NonVirtual float_ "GetTopMargin" [] Nothing
  , NonVirtual float_ "GetAfile" [] Nothing
  , NonVirtual float_ "GetXfile" [] Nothing
  , NonVirtual float_ "GetYfile" [] Nothing
  , NonVirtual float_ "GetAstat" [] Nothing
  , NonVirtual float_ "GetXstat" [] Nothing
  , NonVirtual float_ "GetYstat" [] Nothing
  , NonVirtual short_ "GetFrameFillColor" [] Nothing
  , NonVirtual short_ "GetFrameLineColor" [] Nothing
  , NonVirtual short_ "GetFrameFillStyle" [] Nothing
  , NonVirtual short_ "GetFrameLineStyle" [] Nothing
  , NonVirtual short_ "GetFrameLineWidth" [] Nothing
  , NonVirtual short_ "GetFrameBorderSize" [] Nothing
  , NonVirtual short_ "GetFrameBorderMode" [] Nothing
  , Virtual void_ "ResetAttPad" [cstring "option"] Nothing
  , Virtual void_ "SetBottomMargin" [float "bottommargin"] Nothing
  , Virtual void_ "SetLeftMargin" [float "leftmargin"] Nothing
  , Virtual void_ "SetRightMargin" [float "rightmargin"] Nothing
  , Virtual void_ "SetTopMargin" [float "topmargin"] Nothing
  , Virtual void_ "SetMargin" [float "left", float "right", float "bottom", float "top"] Nothing
  , Virtual void_ "SetAfile" [float "afile"] Nothing
  , Virtual void_ "SetXfile" [float "xfile"] Nothing
  , Virtual void_ "SetYfile" [float "yfile"] Nothing
  , Virtual void_ "SetAstat" [float "astat"] Nothing
  , Virtual void_ "SetXstat" [float "xstat"] Nothing
  , Virtual void_ "SetYstat" [float "ystat"] Nothing
  , NonVirtual void_ "SetFrameFillColor" [short "color"] Nothing
  , NonVirtual void_ "SetFrameLineColor" [short "color"] Nothing
  , NonVirtual void_ "SetFrameFillStyle" [short "styl"] Nothing
  , NonVirtual void_ "SetFrameLineStyle" [short "styl"] Nothing
  , NonVirtual void_ "SetFrameLineWidth" [short "width"] Nothing
  , NonVirtual void_ "SetFrameBorderSize" [short "size"] Nothing
  , NonVirtual void_ "SetFrameBorderMode" [int "mode"] Nothing

  ]

{-
tAttParticle :: Class
tAttParticle = coreclass "TAttParticle" [tNamed] mempty 
               [] 
-}

tAttText :: Class
tAttText = 
  coreclass "TAttText" [deletable] mempty
  [ Constructor [int "align", float "angle", short "color", short "font", float "tsize" ] Nothing
  , Virtual short_ "GetTextAlign" [] Nothing
  , Virtual float_ "GetTextAngle" [] Nothing
  , Virtual short_ "GetTextColor" [] Nothing
  , Virtual short_ "GetTextFont" [] Nothing
  , Virtual float_ "GetTextSize" [] Nothing
  , Virtual void_ "ResetAttText" [cstring "toption"] Nothing
  -- SaveTextAttributes
  , Virtual void_ "SetTextAttributes" [] Nothing
  , Virtual void_ "SetTextAlign" [short "align"] Nothing
  , Virtual void_ "SetTextAngle" [float "tangle"] Nothing
  , Virtual void_ "SetTextColor" [int "tcolor"] Nothing
  , Virtual void_ "SetTextFont" [short "tfont"] Nothing
  , Virtual void_ "SetTextSize"  [float "tsize"] Nothing 
  , Virtual void_ "SetTextSizePixels" [int "npixels"] Nothing
  ]  




----------------
-- starting C --
----------------

tClass :: Class
tClass = coreclass "TClass" [tDictionary] mempty
         [
         ]

tCollection :: Class
tCollection = 
  coreclass "TCollection" [tObject] mempty []

----------------
-- starting D --
----------------

tDirectory :: Class
tDirectory = coreclass "TDirectory" [tNamed] mempty
             [ Static  void_ "AddDirectory" [bool "add"] Nothing
             , Static  bool_ "AddDirectoryStatus" [] Nothing
             , Virtual void_ "Append" [cppclass tObject "obj", bool "replace"] Nothing
             , Virtual void_ "Add" [cppclass tObject "obj", bool "replace"] (Just "addD")
             , Virtual int_ "AppendKey" [cppclass tKey "key" ] Nothing
             , Virtual void_ "Close"    [ cstring "option" ] Nothing
             , Virtual (cppclass_ tObject) "Get" [ cstring "namecycle" ] Nothing 
             , Virtual bool_ "cd" [ cstring "path" ]  (Just "cd_TDirectory")
             ]

tDictionary :: Class
tDictionary = AbstractClass corecabal "TDictionary" [tNamed] mempty Nothing 
              [
              ]

----------------
-- starting G --
----------------

tGlobal :: Class
tGlobal = 
  coreclass "TGlobal" [tDictionary] mempty
  [   
  ] 


----------------
-- starting K --
----------------

tKey :: Class
tKey = coreclass "TKey" [tNamed] mempty
       [ 
-- Constructor [cstring "name", cstring "title", cppclass "TClass" "cl", int "nbytes", cppclass "TDirectory" "motherDir"]
       ] 


----------------
-- starting N --
----------------

tNamed :: Class
tNamed = 
  Class corecabal "TNamed" [tObject] mempty Nothing
  [ Constructor [cstring "name", cstring "title"] Nothing
  , Virtual void_  "SetName"      [cstring "name"] Nothing
  , Virtual void_  "SetNameTitle" [cstring "name", cstring "title"] Nothing
  , Virtual void_  "SetTitle"     [cstring "name"] Nothing
  ]

----------------
-- starting O --
----------------

tObjArray :: Class 
tObjArray = 
  coreclass "TObjArray" [tSeqCollection] mempty []


tObject :: Class
tObject = 
  coreclass "TObject" [deletable] mempty
  [ Constructor [] Nothing
  -- , Virtual int_ "DistancetoPrimitive" [int "px", int "py"]
  , Virtual void_    "Draw"    [cstring "option"] Nothing
  -- , Virtual void_ "ExecuteEvent" [int "event", int "px", int "py"]
  , Virtual (cppclass_ tObject) "FindObject" [cstring "name"] Nothing
  , Virtual  cstring_ "GetName" [] Nothing
  , Virtual (cppclass_ tClass) "IsA" [] Nothing
  , Virtual void_ "Paint" [cstring "option"] Nothing
  , Virtual void_ "Print" [cstring "option"] (Just "printObj")
  , Virtual void_    "SaveAs"  [cstring "filename", cstring "option"] Nothing
  , Virtual int_     "Write"   [cstring "name", int "option", int "bufsize" ] Nothing

  , Static  bool_    "GetObjectStat" [] Nothing
  ]

----------------
-- starting Q --
----------------

tQObject :: Class
tQObject = coreclass "TQObject" [deletable] mempty
           []

----------------
-- starting R --
----------------

tROOT :: Class
tROOT = 
  coreclass "TROOT" [tDirectory] mempty 
  [ NonVirtual (cppclass_ tGlobal) "GetGlobal" [ cstring "name", bool "load" ] Nothing
  , Static bool_ "Initialized" [] Nothing
  ]


----------------
-- starting S --
----------------

tSeqCollection :: Class
tSeqCollection = 
  coreclass "TSeqCollection" [tCollection] mempty []

tString :: Class 
tString = 
  coreclass "TString" [] mempty 
  [ Constructor [ cstring "s" ] Nothing
  ] 

tSystem :: Class
tSystem = 
  coreclass "TSystem" [tNamed] mempty
  [ Virtual bool_ "ProcessEvents" [] Nothing
  ]

----------------
-- starting V --
----------------

tVirtualPad :: Class 
tVirtualPad = coreclass "TVirtualPad" [tObject] mempty
              [ Virtual self_ "cd" [int "subpadnumber"] Nothing
              , Virtual void_ "Divide" [int "nx", int "ny", float "xmargin", float "ymargin", int "color" ] (Just "divide_tvirtualpad")
              , Virtual void_ "SetLogx" [int "value"] Nothing
              , Virtual void_ "SetLogy" [int "value"] Nothing
              , Virtual void_ "SetLogz" [int "value"] Nothing
              ] 

core_classes :: [Class] 
core_classes = 
  [ deletable
  , tApplication, tArray, tArrayC, tArrayD, tArrayF, tArrayI, tArrayL, tArrayL64, tArrayS, tAtt3D, tAttAxis, tAttBBox, tAttCanvas, tAttFill, tAttLine, tAttMarker, tAttPad, tAttText
  , tClass, tCollection
  , tDictionary, tDirectory
  , tGlobal
  , tKey
  , tNamed
  , tObjArray, tObject
  , tQObject
  , tROOT
  , tSeqCollection, tSystem
  , tVirtualPad
  ] 

core_topfunctions = 
  [ TopLevelFunction (cppclass_ tROOT) "GetROOT" [] Nothing
  , TopLevelVariable (cppclass_ tROOT) "gROOT" Nothing
  , TopLevelVariable (cppclass_ tSystem) "gSystem" Nothing
  ] 






