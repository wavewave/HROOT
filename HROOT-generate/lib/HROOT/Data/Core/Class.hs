{-# LANGUAGE OverloadedStrings #-}

module HROOT.Data.Core.Class where

import FFICXX.Generate.Code.Primitive ( bool    , bool_
                                      , charpp
                                      , cppclass, cppclass_
                                      , cstar_
                                      , cstring , cstring_
                                      , double  , double_
                                      , float   , float_
                                      , int     , int_
                                      , intp
                                      , self_
                                      , short   , short_
                                      , void_
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , CTypes(CTDouble)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import FFICXX.Generate.Type.PackageInterface ( Namespace(..), HeaderName(..) )


corecabal :: Cabal
corecabal = Cabal {
    cabal_pkgname            = CabalName "HROOT-core"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTCore"
  , cabal_moduleprefix       = "HROOT.Core"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Custom [CabalName "Cabal", CabalName "base", CabalName "process"]
  }

coreclass :: String -> [Class] -> [Function] -> Class
coreclass n ps fs =
  Class {
      class_cabal      = corecabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    }

modImports ::
     String
  -> [String]
  -> [HeaderName]
  -> (ModuleUnit,ModuleUnitImports)
modImports n ns hs =
  ( MU_Class n
  , ModuleUnitImports {
      muimports_namespaces = map NS ns
    , muimports_headers    = hs
    }
  )

deletable :: Class
deletable =
  AbstractClass {
      class_cabal      = corecabal
    , class_name       = "Deletable"
    , class_parents    = []
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = [ Destructor Nothing ]
    , class_vars       = []
    , class_tmpl_funcs = []
    }

----------------
-- starting A --
----------------

tApplication :: Class
tApplication = coreclass "TApplication" [tObject, tQObject]
               [ Constructor    [ cstring "appClassName", intp "argc", charpp "argv"  ] Nothing
               , Virtual void_ "Run"    [ bool "retrn"]  Nothing
               ]

tArray :: Class
tArray =
  coreclass "TArray" [deletable]
  [ Virtual double_ "GetAt" [ int "i" ] Nothing
  , NonVirtual int_ "GetSize" [] Nothing
  , Virtual void_ "Set" [ int "n" ] (Just "SetArray")
  , Virtual void_ "SetAt" [ double "v", int "i" ] Nothing
  ]

tArrayC :: Class
tArrayC = coreclass "TArrayC" [tArray]
          []

tArrayD :: Class
tArrayD =
  coreclass "TArrayD" [tArray]
  [ NonVirtual double_ "At" [ int "i" ] Nothing
  , NonVirtual (cstar_ CTDouble) "GetArray" [] Nothing
  ]

tArrayF :: Class
tArrayF = coreclass "TArrayF" [tArray]
          []

tArrayI :: Class
tArrayI = coreclass "TArrayI" [tArray]
          []

tArrayL :: Class
tArrayL = coreclass "TArrayL" [tArray]
          []

tArrayL64 :: Class
tArrayL64 = coreclass "TArrayL64" [tArray]
            []

tArrayS :: Class
tArrayS = coreclass "TArrayS" [tArray]
          []

tAtt3D :: Class
tAtt3D = coreclass "TAtt3D" [deletable]
         []

tAttAxis :: Class
tAttAxis =
  coreclass "TAttAxis" [deletable]
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
tAttBBox = coreclass "TAttBBox" [deletable]
           []

tAttBBox2D :: Class
tAttBBox2D =
  AbstractClass {
      class_cabal      = corecabal
    , class_name       = "TAttBBox2D"
    , class_parents    = [deletable]
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = []
    , class_vars       = []
    , class_tmpl_funcs = []
    }

tAttCanvas :: Class
tAttCanvas = coreclass "TAttCanvas" [deletable]
             [ Constructor []  Nothing
             ]

tAttFill :: Class
tAttFill = coreclass "TAttFill" [deletable]
           [ Constructor [short "fcolor", short "fstyle"] Nothing
           , Virtual void_   "SetFillColor"    [int "color" ] Nothing
           , Virtual void_   "SetFillStyle"    [int "style" ] Nothing
           ]

tAttLine :: Class
tAttLine =
  coreclass "TAttLine" [deletable]
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
  coreclass "TAttMarker" [deletable]
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
  coreclass "TAttPad" [deletable]
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
tAttParticle = coreclass "TAttParticle" [tNamed]
               []
-}

tAttText :: Class
tAttText =
  coreclass "TAttText" [deletable]
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
tClass = coreclass "TClass" [tDictionary]
         [
         ]

tCollection :: Class
tCollection =
  coreclass "TCollection" [tObject] []

----------------
-- starting D --
----------------

tDictionary :: Class
tDictionary =
  AbstractClass {
      class_cabal      = corecabal
    , class_name       = "TDictionary"
    , class_parents    = [tNamed]
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = []
    , class_vars       = []
    , class_tmpl_funcs = []
    }

tDirectory :: Class
tDirectory = coreclass "TDirectory" [tNamed]
             [ Static  void_ "AddDirectory" [bool "add"] Nothing
             , Static  bool_ "AddDirectoryStatus" [] Nothing
             , Virtual void_ "Append" [cppclass tObject "obj", bool "replace"] Nothing
             , Virtual void_ "Add" [cppclass tObject "obj", bool "replace"] (Just "addD")
             , Virtual int_ "AppendKey" [cppclass tKey "key" ] Nothing
             , Virtual void_ "Close"    [ cstring "option" ] Nothing
             , Virtual (cppclass_ tObject) "Get" [ cstring "namecycle" ] Nothing
             , Virtual bool_ "cd" [ cstring "path" ]  (Just "cd_TDirectory")
             ]

----------------
-- starting G --
----------------

tGlobal :: Class
tGlobal =
  coreclass "TGlobal" [tDictionary]
  [
  ]


----------------
-- starting K --
----------------

tKey :: Class
tKey = coreclass "TKey" [tNamed]
  [ Constructor [ cstring "name", cstring "title", cppclass tClass "cl", int "nbytes"
                , cppclass tDirectory "motherDir"] Nothing
  ]


----------------
-- starting N --
----------------

tNamed :: Class
tNamed =
  coreclass "TNamed" [tObject]
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
  coreclass "TObjArray" [tSeqCollection]
  []


tObject :: Class
tObject =
  coreclass "TObject" [deletable]
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
tQObject =
  coreclass "TQObject" [deletable]
  []

----------------
-- starting R --
----------------

tROOT :: Class
tROOT =
  coreclass "TROOT" [tDirectory]
  [ NonVirtual (cppclass_ tGlobal) "GetGlobal" [ cstring "name", bool "load" ] Nothing
  , Static bool_ "Initialized" [] Nothing
  ]


----------------
-- starting S --
----------------

tSeqCollection :: Class
tSeqCollection =
  coreclass "TSeqCollection" [tCollection]
  []

tString :: Class
tString =
  coreclass "TString" []
  [ Constructor [ cstring "s" ] Nothing
  ]

tStyle :: Class
tStyle =
  coreclass "TStyle" [tNamed, tAttLine, tAttFill, tAttMarker, tAttText]
  [ NonVirtual void_ "SetCanvasPreferGL" [bool "prefer"] Nothing
  ]

tSystem :: Class
tSystem =
  coreclass "TSystem" [tNamed]
  [ Virtual bool_ "ProcessEvents" [] Nothing
  ]

----------------
-- starting V --
----------------

tVirtualPad :: Class
tVirtualPad =
  coreclass "TVirtualPad" [tObject]
  [ Virtual self_ "cd" [int "subpadnumber"] Nothing
  , Virtual void_ "Divide" [int "nx", int "ny", float "xmargin", float "ymargin", int "color" ] (Just "divide_tvirtualpad")
  , Virtual void_ "Range" [double "x1", double "y1", double "x2", double "y2"] Nothing
  , Virtual void_ "SetLogx" [int "value"] Nothing
  , Virtual void_ "SetLogy" [int "value"] Nothing
  , Virtual void_ "SetLogz" [int "value"] Nothing
  , Virtual void_ "Update"  [] Nothing
  ]

core_classes :: [Class]
core_classes =
  [ deletable
  , tApplication, tArray, tArrayC, tArrayD, tArrayF, tArrayI, tArrayL, tArrayL64, tArrayS
  , tAtt3D, tAttAxis, tAttBBox, tAttBBox2D, tAttCanvas, tAttFill, tAttLine, tAttMarker, tAttPad, tAttText
  , tClass, tCollection
  , tDictionary, tDirectory
  , tGlobal
  , tKey
  , tNamed
  , tObjArray, tObject
  , tQObject
  , tROOT
  , tSeqCollection, tStyle, tSystem
  , tVirtualPad
  ]

core_topfunctions :: [TopLevelFunction]
core_topfunctions =
  [ TopLevelFunction (cppclass_ tROOT)   "GetROOT" [] Nothing
  , TopLevelVariable (cppclass_ tROOT)   "gROOT" Nothing
  , TopLevelVariable (cppclass_ tSystem) "gSystem" Nothing
  , TopLevelVariable (cppclass_ tStyle)  "gStyle" Nothing
  ]

core_headers :: [(ModuleUnit,ModuleUnitImports)]
core_headers =
  [ modImports "TApplication"   ["ROOT"] ["TApplication.h"]
  , modImports "TArray"         ["ROOT"] ["TArray.h"]
  , modImports "TArrayC"        ["ROOT"] ["TArrayC.h"]
  , modImports "TArrayD"        ["ROOT"] ["TArrayD.h"]
  , modImports "TArrayF"        ["ROOT"] ["TArrayF.h"]
  , modImports "TArrayI"        ["ROOT"] ["TArrayI.h"]
  , modImports "TArrayL"        ["ROOT"] ["TArrayL.h"]
  , modImports "TArrayL64"      ["ROOT"] ["TArrayL64.h"]
  , modImports "TArrayS"        ["ROOT"] ["TArrayS.h"]
  , modImports "TAtt3D"         ["ROOT"] ["TAtt3D.h"]
  , modImports "TAttAxis"       ["ROOT"] ["TAttAxis.h"]
  , modImports "TAttBBox"       ["ROOT"] ["TAttBBox.h"]
  , modImports "TAttBBox2D"     ["ROOT"] ["TAttBBox2D.h"]
  , modImports "TAttCanvas"     ["ROOT"] ["TAttCanvas.h"]
  , modImports "TAttFill"       ["ROOT"] ["TAttFill.h"]
  , modImports "TAttLine"       ["ROOT"] ["TAttLine.h"]
  , modImports "TAttMarker"     ["ROOT"] ["TAttMarker.h"]
  , modImports "TAttPad"        ["ROOT"] ["TAttPad.h"]
  , modImports "TAttText"       ["ROOT"] ["TAttText.h"]
  , modImports "TClass"         ["ROOT"] ["TClass.h"]
  , modImports "TCollection"    ["ROOT"] ["TCollection.h"]
  , modImports "TDictionary"    ["ROOT"] ["TDictionary.h"]
  , modImports "TDirectory"     ["ROOT"] ["TDirectory.h"]
  , modImports "TGlobal"        ["ROOT"] ["TGlobal.h"]
  , modImports "TKey"           ["ROOT"] ["TKey.h"]
  , modImports "TNamed"         ["ROOT"] ["TNamed.h"]
  , modImports "TObjArray"      ["ROOT"] ["TObjArray.h"]
  , modImports "TObject"        ["ROOT"] ["TObject.h"]
  , modImports "TQObject"       ["ROOT"] ["TQObject.h"]
  , modImports "TROOT"          ["ROOT"] ["TROOT.h"]
  , modImports "TSeqCollection" ["ROOT"] ["TSeqCollection.h"]
  , modImports "TStyle"         ["ROOT"] ["TStyle.h"]
  , modImports "TSystem"        ["ROOT"] ["TSystem.h"]
  , modImports "TVirtualPad"    ["ROOT"] ["TVirtualPad.h"]
  ]

core_extraLib :: [String]
core_extraLib = []

core_extraDep :: [(String,[String])]
core_extraDep = []
