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
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module


corecabal = Cabal { cabal_pkgname = "HROOT-core"
                  , cabal_cheaderprefix = "HROOTCore" 
                  , cabal_moduleprefix = "HROOT.Core" 
                  } 

coreclass = Class corecabal 

moduleInterface :: Module
moduleInterface = Module { module_name = "HROOT.Class.Interface"
                         , module_exports = [ "IDeletable" 
                                            , "TObject"
                                            , "ITObject"
                                            , "ITNamed" 
                                            , "TNamed" ]
                         }  

deletable :: Class 
deletable = AbstractClass corecabal "Deletable" [] mempty 
          [ Destructor ]


----------------
-- starting A --
----------------


tApplication :: Class
tApplication = coreclass "TApplication" [tObject, tQObject] mempty
               [ Constructor    [ cstring "appClassName", intp "argc", charpp "argv"  ] 
               , Virtual void_ "Run"    [ bool "retrn"]   
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
  [ Constructor [] 
  , Virtual int_ "GetNdivisions" [] 
  , Virtual short_ "GetAxisColor" [] 
  , Virtual short_ "GetLabelColor" []
  , Virtual short_ "GetLabelFont" [] 
  , Virtual float_ "GetLabelOffset" [] 
  , Virtual float_ "GetLabelSize" [] 
  , Virtual float_ "GetTitleOffset" [] 
  , Virtual float_ "GetTitleSize" [] 
  , Virtual float_ "GetTickLength" []
  , Virtual short_ "GetTitleFont" [] 
  -- omit..
  , Virtual void_   "SetNdivisions"   [int "n", bool "optim" ]
  , Virtual void_   "SetAxisColor"    [short  "color"]
  , Virtual void_   "SetLabelColor"   [short  "color" ] 
  , Virtual void_   "SetLabelFont"    [short "font"] 
  , Virtual void_   "SetLabelOffset"  [float "offset"] 
  , Virtual void_   "SetLabelSize"    [float  "size"  ] 
  , Virtual void_   "SetTickLength"   [float  "length" ] 
  , Virtual void_   "SetTitleOffset"  [float  "offset" ] 
  , Virtual void_   "SetTitleSize"    [float  "size"]
  , Virtual void_   "SetTitleColor"   [short  "color"]
  , Virtual void_   "SetTitleFont"    [short  "font"]

  ] 


tAttBBox :: Class 
tAttBBox = coreclass "TAttBBox" [deletable] mempty 
           []

tAttCanvas :: Class
tAttCanvas = coreclass "TAttCanvas" [deletable] mempty
             [ Constructor []  
             ]

tAttFill :: Class
tAttFill = coreclass "TAttFill" [deletable] mempty
           [ Constructor [short "fcolor", short "fstyle"] 
           , Virtual void_   "SetFillColor"    [int "color" ] 
           , Virtual void_   "SetFillStyle"    [int "style" ]  
           ]

tAttLine :: Class
tAttLine = 
  coreclass "TAttLine" [deletable] mempty
  [ Constructor [short "lcolor", short "lstyle", short "lwidth"] 
  , NonVirtual int_ "DistancetoLine" [int "px", int "py", double "xp1", double "yp1", double "xp2", double "yp2"]
  , Virtual short_ "GetLineColor" [] 
  , Virtual short_ "GetLineStyle" [] 
  , Virtual short_ "GetLineWidth" [] 
  --   , Virtual void_ "Modify" [] 
  , Virtual void_ "ResetAttLine" [cstring "option"]
  -- SaveLineAttributes
  , Virtual void_ "SetLineAttributes" [] 
  , Virtual void_ "SetLineColor" [short "lcolor" ] 
  , Virtual void_ "SetLineStyle" [short "lstyle" ]
  , Virtual void_ "SetLineWidth" [short "lwidth" ]
  ]


tAttMarker :: Class
tAttMarker = 
  coreclass "TAttMarker" [deletable] mempty
  [ Constructor [short "color", short "style", short "msize"] 
  , Virtual short_ "GetMarkerColor" [] 
  , Virtual short_ "GetMarkerStyle" [] 
  , Virtual float_ "GetMarkerSize" []
  -- Modify
  , Virtual void_ "ResetAttMarker" [cstring "option"]
  , Virtual void_ "SetMarkerAttributes" [] 
  , Virtual void_ "SetMarkerColor" [short "tcolor"] 
  , Virtual void_ "SetMarkerStyle" [short "mstyle"]
  , Virtual void_ "SetMarkerSize" [short "msize"] 
  ]  

tAttPad :: Class
tAttPad = 
  coreclass "TAttPad" [deletable] mempty
  [ Constructor [] 
  , NonVirtual float_ "GetBottomMargin" [] 
  , NonVirtual float_ "GetLeftMargin" [] 
  , NonVirtual float_ "GetRightMargin" [] 
  , NonVirtual float_ "GetTopMargin" [] 
  , NonVirtual float_ "GetAfile" [] 
  , NonVirtual float_ "GetXfile" [] 
  , NonVirtual float_ "GetYfile" [] 
  , NonVirtual float_ "GetAstat" [] 
  , NonVirtual float_ "GetXstat" [] 
  , NonVirtual float_ "GetYstat" [] 
  , NonVirtual short_ "GetFrameFillColor" [] 
  , NonVirtual short_ "GetFrameLineColor" [] 
  , NonVirtual short_ "GetFrameFillStyle" [] 
  , NonVirtual short_ "GetFrameLineStyle" [] 
  , NonVirtual short_ "GetFrameLineWidth" [] 
  , NonVirtual short_ "GetFrameBorderSize" [] 
  , NonVirtual short_ "GetFrameBorderMode" [] 
  , Virtual void_ "ResetAttPad" [cstring "option"] 
  , Virtual void_ "SetBottomMargin" [float "bottommargin"] 
  , Virtual void_ "SetLeftMargin" [float "leftmargin"]
  , Virtual void_ "SetRightMargin" [float "rightmargin"]
  , Virtual void_ "SetTopMargin" [float "topmargin"]
  , Virtual void_ "SetMargin" [float "left", float "right", float "bottom", float "top"]
  , Virtual void_ "SetAfile" [float "afile"] 
  , Virtual void_ "SetXfile" [float "xfile"]
  , Virtual void_ "SetYfile" [float "yfile"]
  , Virtual void_ "SetAstat" [float "astat"]
  , Virtual void_ "SetXstat" [float "xstat"]
  , Virtual void_ "SetYstat" [float "ystat"]
  , NonVirtual void_ "SetFrameFillColor" [short "color"]
  , NonVirtual void_ "SetFrameLineColor" [short "color"]
  , NonVirtual void_ "SetFrameFillStyle" [short "styl"]
  , NonVirtual void_ "SetFrameLineStyle" [short "styl"]
  , NonVirtual void_ "SetFrameLineWidth" [short "width"]
  , NonVirtual void_ "SetFrameBorderSize" [short "size"]
  , NonVirtual void_ "SetFrameBorderMode" [int "mode"]

  ]

{-
tAttParticle :: Class
tAttParticle = coreclass "TAttParticle" [tNamed] mempty 
               [] 
-}

tAttText :: Class
tAttText = 
  coreclass "TAttText" [deletable] mempty
  [ Constructor [int "align", float "angle", short "color", short "font", float "tsize" ] 
  , Virtual short_ "GetTextAlign" [] 
  , Virtual float_ "GetTextAngle" [] 
  , Virtual short_ "GetTextColor" [] 
  , Virtual short_ "GetTextFont" [] 
  , Virtual float_ "GetTextSize" [] 
  , Virtual void_ "ResetAttText" [cstring "toption"] 
  -- SaveTextAttributes
  , Virtual void_ "SetTextAttributes" [] 
  , Virtual void_ "SetTextAlign" [short "align"]
  , Virtual void_ "SetTextAngle" [float "tangle"]
  , Virtual void_ "SetTextColor" [int "tcolor"]  
  , Virtual void_ "SetTextFont" [short "tfont"]
  , Virtual void_ "SetTextSize"  [float "tsize"]  
  , Virtual void_ "SetTextSizePixels" [int "npixels"]
  ]  




----------------
-- starting C --
----------------

tClass :: Class
tClass = Class corecabal "TClass" [tDictionary] mempty
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
             [ Static  void_ "AddDirectory" [bool "add"]
             , Static  bool_ "AddDirectoryStatus" []
             , Virtual void_ "Append" [cppclass tObject "obj", bool "replace"]
             , AliasVirtual void_ "Add" [cppclass tObject "obj", bool "replace"] "addD"
             , Virtual int_ "AppendKey" [cppclass tKey "key" ] 
             , Virtual void_ "Close"    [ cstring "option" ] 
             , Virtual (cppclass_ tObject) "Get" [ cstring "namecycle" ] 
             ]

tDictionary :: Class
tDictionary = AbstractClass corecabal "TDictionary" [tNamed] mempty
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
  Class corecabal "TNamed" [tObject] mempty
  [ Constructor [cstring "name", cstring "title"] 
  , Virtual void_  "SetName"      [cstring "name"]
  , Virtual void_  "SetNameTitle" [cstring "name", cstring "title"]
  , Virtual void_  "SetTitle"     [cstring "name"]  
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
  [ Constructor [] 
  -- , Virtual int_ "DistancetoPrimitive" [int "px", int "py"]
  , Virtual void_    "Draw"    [cstring "option"] 
  -- , Virtual void_ "ExecuteEvent" [int "event", int "px", int "py"]
  , Virtual (cppclass_ tObject) "FindObject" [cstring "name"]
  , Virtual  cstring_ "GetName" [] 
  , Virtual (cppclass_ tClass) "IsA" [] 
  , Virtual void_ "Paint" [cstring "option"] 
  , AliasVirtual void_ "Print" [cstring "option"] "printObj"
  , Virtual void_    "SaveAs"  [cstring "filename", cstring "option"] 
  , Virtual int_     "Write"   [cstring "name", int "option", int "bufsize" ]

  , Static  bool_    "GetObjectStat" []
  ]

----------------
-- starting Q --
----------------

tQObject :: Class
tQObject = coreclass "TQObject" [deletable] mempty
           []


----------------
-- starting S --
----------------

tSeqCollection :: Class
tSeqCollection = 
  coreclass "TSeqCollection" [tCollection] mempty []

tString :: Class 
tString = 
  coreclass "TString" [] mempty 
  [ Constructor [ cstring "s" ] 
  ] 

----------------
-- starting V --
----------------

tVirtualPad :: Class 
tVirtualPad = coreclass "TVirtualPad" [tObject] mempty
              [ Virtual self_ "cd" [int "subpadnumber"] 
              , AliasVirtual void_ "Divide" [int "nx", int "ny", float "xmargin", float "ymargin", int "color" ] "divide_tvirtualpad"
              , Virtual void_ "SetLogx" [int "value"]  
              , Virtual void_ "SetLogy" [int "value"]
              , Virtual void_ "SetLogz" [int "value"]
              ] 




core_classes :: [Class] 
core_classes = 
  [ deletable
  , tApplication, tArray, tArrayC, tArrayD, tArrayF, tArrayI, tArrayL, tArrayL64, tArrayS, tAtt3D, tAttAxis, tAttBBox, tAttCanvas, tAttFill, tAttLine, tAttMarker, tAttPad, tAttText
  , tClass, tCollection
  , tDictionary, tDirectory
  , tKey
  , tNamed
  , tObjArray, tObject
  , tQObject
  , tSeqCollection
  , tVirtualPad
  ] 








