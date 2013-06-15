-- |
-- Module      : HROOT.Data.Graf.Class
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- 
-- License     : LGPL-2
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Graf.Class where

import Data.Monoid
--
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
-- 
import HROOT.Data.Core.Class
import HROOT.Data.Hist.Class

grafcabal = Cabal { cabal_pkgname = "HROOT-graf"
                  , cabal_cheaderprefix = "HROOTGraf" 
                  , cabal_moduleprefix = "HROOT.Graf" } 

grafclass n ps ann fs = Class grafcabal n ps ann Nothing fs 

----------------
-- starting A --
----------------

tArc :: Class 
tArc = 
  grafclass "TArc" [tEllipse] mempty
  [ Constructor [double "x1", double "y1", double "radius", double "phimin", double "phimax" ] Nothing
  ]

tArrow :: Class
tArrow = 
  grafclass "TArrow" [tLine, tAttFill] mempty
  [ Constructor [double "x1", double "y1", double "x2", double "y2", float "arrowsize", cstring "option" ] Nothing
  ]

tAttImage :: Class
tAttImage = grafclass "TAttImage" [deletable] mempty
            []

----------------
-- starting B --
----------------

tBRIK :: Class
tBRIK = 
  grafclass "TBRIK" [tShape] mempty 
  [ Constructor [cstring "name", cstring "title", cstring "material", float "dx", float "dy", float "dz" ] Nothing
  ]

----------------
-- starting C --
----------------

tCanvas :: Class
tCanvas = grafclass "TCanvas" [tPad] mempty
          [ Constructor [cstring "name",cstring "title",int "ww",int "wh"] Nothing
          ] 

tCrown :: Class
tCrown = 
  grafclass "TCrown" [tEllipse] mempty
  [ Constructor [double "x1", double "y1", double "radin", double "radout", double "phimin", double "phimax"] Nothing
  ]


tCutG :: Class
tCutG = 
  grafclass "TCutG" [tGraph] mempty 
  [ Constructor [cstring "name", int "n", doublep "x", doublep "y"] Nothing
  ]

----------------
-- starting E --
----------------

tEllipse :: Class
tEllipse = 
  grafclass "TEllipse" [tObject, tAttLine, tAttFill] mempty
  [ Constructor [double "x1", double "y1", double "r1", double "r2", double "phimin", double "phimax", double "theta" ] Nothing
  ]

----------------
-- starting G --
----------------

tGaxis :: Class 
tGaxis = 
  grafclass "TGaxis" [tLine, tAttText] mempty
  [ Constructor [double "xmin", double "ymin", double "xmax", double "ymax", double "wmin", double "wmax", int "ndiv", cstring "chopt", double "gridlength" ] Nothing
  ]


tGraphPolar :: Class
tGraphPolar = 
  grafclass "TGraphPolar" [tGraphErrors] mempty 
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] Nothing
  ]

tGraphQQ :: Class
tGraphQQ = 
  grafclass "TGraphQQ" [tGraph] mempty
  [ Constructor [int "nx", doublep "x", int "ny", doublep "y"] Nothing
  ]


----------------
-- starting L --
----------------

tLine :: Class
tLine = 
  grafclass "TLine" [tObject, tAttLine] mempty
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
-- starting P --
----------------

tPad :: Class
tPad = 
  grafclass "TPad" [tVirtualPad] mempty
  [ 
  ] 

tPCON :: Class
tPCON = 
  grafclass "TPCON" [tShape] mempty
  [ Constructor [cstring "name", cstring "title", cstring "material", float "phi1", float "dphi1", int "nz"] Nothing
  ]

----------------
-- starting S --
----------------

tShape :: Class 
tShape = 
  grafclass "TShape" [tNamed, tAttLine, tAttFill, tAtt3D] mempty
  [ Constructor [cstring "name", cstring "title", cstring "material" ] Nothing 
  ]

tSPHE :: Class
tSPHE = 
  grafclass "TSPHE" [tShape] mempty 
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "themin", float "themax", float "phimin", float "phimax" ] Nothing
  ]

----------------
-- starting T --
----------------

tTUBE :: Class
tTUBE = 
  grafclass "TTUBE" [tShape] mempty
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "dz", float "aspect"] Nothing
  ]


graf_classes :: [Class]
graf_classes = 
  [ tArc, tArrow, tAttImage
  , tBRIK
  , tCanvas, tCrown, tCutG
  , tEllipse
  , tGaxis, tGraphPolar, tGraphQQ
  , tLine
  , tPad, tPCON
  , tShape, tSPHE
  , tTUBE
  ] 

graf_topfunctions = [ ] 
