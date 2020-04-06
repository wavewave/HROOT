{-# LANGUAGE OverloadedStrings #-}

module HROOT.Data.Graf.Class where

import FFICXX.Generate.Code.Primitive ( bool, bool_
                                      , cppclass, cppclass_
                                      , cstring
                                      , double, double_, doublep
                                      , float
                                      , int, intref
                                      , void_
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevel(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      , modImports
                                      )
import HROOT.Data.Core.Class          ( deletable
                                      , tAtt3D, tAttBBox2D, tAttFill, tAttLine, tAttMarker, tAttText
                                      , tNamed
                                      , tObject
                                      , tVirtualPad
                                      )
import HROOT.Data.Hist.Class          ( tGraph, tGraphErrors, tH1F )



grafcabal :: Cabal
grafcabal =
  Cabal {
    cabal_pkgname            = CabalName "HROOT-graf"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTGraf"
  , cabal_moduleprefix       = "HROOT.Graf"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "stdcxx"
                               , CabalName "HROOT-core"
                               , CabalName "HROOT-hist"
                               ]
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Custom [CabalName "Cabal", CabalName "base", CabalName "process"]
  }


grafclass :: String -> [Class] -> [Function] -> Class
grafclass n ps fs =
  Class {
    class_cabal      = grafcabal
  , class_name       = n
  , class_parents    = ps
  , class_protected  = Protected []
  , class_alias      = Nothing
  , class_funcs      = fs
  , class_vars       = []
  , class_tmpl_funcs = []
  , class_has_proxy  = False
  }

----------------
-- starting A --
----------------

tArc :: Class
tArc =
  grafclass "TArc" [tEllipse]
  [ Constructor [double "x1", double "y1", double "radius", double "phimin", double "phimax" ] Nothing
  ]

tArrow :: Class
tArrow =
  grafclass "TArrow" [tLine, tAttFill]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", float "arrowsize", cstring "option" ] Nothing
  ]

tAttImage :: Class
tAttImage =
  grafclass "TAttImage" [deletable]
  []

----------------
-- starting B --
----------------

tBox :: Class
tBox =
  grafclass "TBox" [tObject, tAttLine, tAttFill]
  [ Constructor [double "x1", double "y1", double "x2", double "y2"]
  ]

tBRIK :: Class
tBRIK =
  grafclass "TBRIK" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "dx", float "dy", float "dz" ] Nothing
  ]

----------------
-- starting C --
----------------

tCanvas :: Class
tCanvas =
  grafclass "TCanvas" [tPad]
  [ Constructor [cstring "name",cstring "title",int "ww",int "wh"] Nothing
  , Virtual void_ "ToggleEditor"      [] Nothing
  , Virtual void_ "ToggleEventStatus" [] Nothing
  , Virtual void_ "ToggleToolBar"     [] Nothing
  , Virtual void_ "ToggleToolTips"    [] Nothing
  ]

tCrown :: Class
tCrown =
  grafclass "TCrown" [tEllipse]
  [ Constructor [double "x1", double "y1", double "radin", double "radout", double "phimin", double "phimax"] Nothing
  ]


tCutG :: Class
tCutG =
  grafclass "TCutG" [tGraph]
  [ Constructor [cstring "name", int "n", doublep "x", doublep "y"] Nothing
  ]

----------------
-- starting E --
----------------

tEllipse :: Class
tEllipse =
  grafclass "TEllipse" [tObject, tAttLine, tAttFill]
  [ Constructor [double "x1", double "y1", double "r1", double "r2", double "phimin", double "phimax", double "theta" ] Nothing
  ]

----------------
-- starting G --
----------------

tGaxis :: Class
tGaxis =
  grafclass "TGaxis" [tLine, tAttText]
  [ Constructor [double "xmin", double "ymin", double "xmax", double "ymax", double "wmin", double "wmax", int "ndiv", cstring "chopt", double "gridlength" ] Nothing
  ]


tGraphPolar :: Class
tGraphPolar =
  grafclass "TGraphPolar" [tGraphErrors]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] Nothing
  ]

tGraphQQ :: Class
tGraphQQ =
  grafclass "TGraphQQ" [tGraph]
  [ Constructor [int "nx", doublep "x", int "ny", doublep "y"] Nothing
  ]


----------------
-- starting L --
----------------

tLine :: Class
tLine =
  grafclass "TLine" [tObject, tAttLine]
  [ Constructor [double "x1", double "y1", double "x2", double "y2" ] Nothing
  , Virtual (cppclass_ tLine) "DrawLine" [double "x1", double "y1", double "x2", double "y2"] Nothing
  , Virtual (cppclass_ tLine) "DrawLineNDC" [double "x1", double "y1", double "x2", double "y2"] Nothing
  , NonVirtual double_ "GetX1" [] Nothing
  , NonVirtual double_ "GetX2" [] Nothing
  , NonVirtual double_ "GetY1" [] Nothing
  , NonVirtual double_ "GetY2" [] Nothing
  , NonVirtual bool_ "IsHorizontal" [] Nothing
  , NonVirtual bool_ "IsVertical" [] Nothing
  , Virtual void_ "PaintLine" [double "x1", double "y1", double "x2", double "y2"] Nothing
  , Virtual void_ "PaintLineNDC" [double "u1", double "v1", double "u2", double "v2"] Nothing
  , NonVirtual void_ "SetHorizontal" [bool "set"] Nothing
  , NonVirtual void_ "SetVertical" [bool "set"] Nothing
  , Virtual void_ "SetX1" [double "x1"] Nothing
  , Virtual void_ "SetX2" [double "x2"] Nothing
  , Virtual void_ "SetY1" [double "y1"] Nothing
  , Virtual void_ "SetY2" [double "y2"] Nothing
  ]

----------------
-- starting M --
----------------

tMarker :: Class
tMarker =
  grafclass "TMarker" [tObject, tAttMarker, tAttBBox2D]
  [ Constructor [double "x", double "y", int "marker" ] Nothing
  , NonVirtual double_ "GetX" [] Nothing
  , NonVirtual double_ "GetY" [] Nothing
  , Virtual void_ "SetX" [double "x"] Nothing
  , Virtual void_ "SetY" [double "y"] Nothing
  ]

----------------
-- starting P --
----------------

tPad :: Class
tPad =
  grafclass "TPad" [tVirtualPad]
  [ Constructor [cstring "name", cstring "title", double "xlow", double "ylow", double "xup", double "yup"] Nothing
  , Virtual (cppclass_ tH1F) "DrawFrame" [double "xmin", double "ymin", double "xmax", double "ymax", cstring "title"] Nothing
  , Virtual (cppclass_ tView) "GetView" [] Nothing
  , Virtual void_ "SetView" [] (Just "SetView0")
  , Virtual void_ "SetView" [ cppclass tView "view" ] Nothing
  ]



tPCON :: Class
tPCON =
  grafclass "TPCON" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "phi1", float "dphi1", int "nz"] Nothing
  ]

----------------
-- starting S --
----------------

tShape :: Class
tShape =
  grafclass "TShape" [tNamed, tAttLine, tAttFill, tAtt3D]
  [ Constructor [cstring "name", cstring "title", cstring "material" ] Nothing
  ]

tSPHE :: Class
tSPHE =
  grafclass "TSPHE" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "themin", float "themax", float "phimin", float "phimax" ] Nothing
  ]

----------------
-- starting T --
----------------

tTUBE :: Class
tTUBE =
  grafclass "TTUBE" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "dz", float "aspect"] Nothing
  ]

----------------
-- starting V --
----------------

tView :: Class
tView =
  Class
  { class_cabal = grafcabal
  , class_name  = "TView"
  , class_parents = [tObject, tAttLine]
  , class_protected = Protected []
  , class_alias = Nothing
  , class_funcs =
      [ Virtual void_   "Front"          [] Nothing
      , Virtual void_   "FrontView"      [cppclass tVirtualPad "pad"] Nothing
      , Virtual double_ "GetLatitude"    [] Nothing
      , Virtual double_ "GetLongitude"   [] Nothing
      , Virtual double_ "GetPsi"         [] Nothing
      , Virtual bool_   "IsViewChanged"  [] Nothing
      , Virtual void_   "RotateView"     [double "phi", double "theta"] Nothing
      , Virtual void_   "SetLatitude"    [double "latitude" ] Nothing
      , Virtual void_   "SetLongitude"   [double "longitude"] Nothing
      , Virtual void_   "SetPsi"         [double "psi"      ] Nothing
      , Virtual void_   "SetView"        [double "longitude", double "latitude", double "psi", intref "irep"] (Just "SetView1")
      , Virtual void_   "SetViewChanged" [bool "flag"] Nothing
      , Virtual void_   "Side"           [] Nothing
      , Virtual void_   "SideView"       [cppclass tVirtualPad "pad"] Nothing
      , Virtual void_   "Top"            [] Nothing
      , Virtual void_   "TopView" [cppclass tVirtualPad "pad"] Nothing
      ]
  , class_vars = []
  , class_tmpl_funcs = []
  , class_has_proxy = False
  }

tView3D :: Class
tView3D =
  grafclass "TView3D" [tView]
  []


graf_classes :: [Class]
graf_classes =
  [ tArc, tArrow, tAttImage
  , tBox, tBRIK
  , tCanvas, tCrown, tCutG
  , tEllipse
  , tGaxis, tGraphPolar, tGraphQQ
  , tLine
  , tMarker
  , tPad, tPCON
  , tShape, tSPHE
  , tTUBE
  , tView, tView3D
  ]

graf_topfunctions :: [TopLevel]
graf_topfunctions = []


graf_headers :: [(ModuleUnit,ModuleUnitImports)]
graf_headers =
  [ modImports "TArc"        ["ROOT"] ["TArc.h"]
  , modImports "TArrow"      ["ROOT"] ["TArrow.h"]
  , modImports "TAttImage"   ["ROOT"] ["TAttImage.h"]
  , modImports "TBRIK"       ["ROOT"] ["TBRIK.h"]
  , modImports "TCanvas"     ["ROOT"] ["TCanvas.h"]
  , modImports "TCrown"      ["ROOT"] ["TCrown.h"]
  , modImports "TCutG"       ["ROOT"] ["TCutG.h"]
  , modImports "TEllipse"    ["ROOT"] ["TEllipse.h"]
  , modImports "TGaxis"      ["ROOT"] ["TGaxis.h"]
  , modImports "TGraphPolar" ["ROOT"] ["TGraphPolar.h"]
  , modImports "TGraphQQ"    ["ROOT"] ["TGraphQQ.h"]
  , modImports "TLine"       ["ROOT"] ["TLine.h"]
  , modImports "TMarker"     ["ROOT"] ["TMarker.h"]
  , modImports "TPad"        ["ROOT"] ["TPad.h"]
  , modImports "TPCON"       ["ROOT"] ["TPCON.h"]
  , modImports "TShape"      ["ROOT"] ["TShape.h"]
  , modImports "TSPHE"       ["ROOT"] ["TSPHE.h"]
  , modImports "TTUBE"       ["ROOT"] ["TTUBE.h"]
  , modImports "TView"       ["ROOT"] ["TView.h"]
  , modImports "TView3D"     ["ROOT"] ["TView3D.h"]
  ]

graf_extraLib :: [String]
graf_extraLib = []

graf_extraDep :: [(String,[String])]
graf_extraDep = []
