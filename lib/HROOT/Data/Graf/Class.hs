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

grafclass = Class grafcabal 

----------------
-- starting A --
----------------

tArc :: Class 
tArc = 
  grafclass "TArc" [tEllipse] mempty
  [ Constructor [double "x1", double "y1", double "radius", double "phimin", double "phimax" ] 
  ]

tArrow :: Class
tArrow = 
  grafclass "TArrow" [tLine, tAttFill] mempty
  [ Constructor [double "x1", double "y1", double "x2", double "y2", float "arrowsize", cstring "option" ] 
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
  [ Constructor [cstring "name", cstring "title", cstring "material", float "dx", float "dy", float "dz" ] 
  ]

----------------
-- starting C --
----------------

tCanvas :: Class
tCanvas = grafclass "TCanvas" [tPad] mempty
          [ Constructor [cstring "name",cstring "title",int "ww",int "wh"] 
          ] 

tCrown :: Class
tCrown = 
  grafclass "TCrown" [tEllipse] mempty
  [ Constructor [double "x1", double "y1", double "radin", double "radout", double "phimin", double "phimax"] 
  ]


tCutG :: Class
tCutG = 
  grafclass "TCutG" [tGraph] mempty 
  [ Constructor [cstring "name", int "n", doublep "x", doublep "y"] 
  ]

----------------
-- starting E --
----------------

tEllipse :: Class
tEllipse = 
  grafclass "TEllipse" [tObject, tAttLine, tAttFill] mempty
  [ Constructor [double "x1", double "y1", double "r1", double "r2", double "phimin", double "phimax", double "theta" ] 
  ]

----------------
-- starting G --
----------------

tGaxis :: Class 
tGaxis = 
  grafclass "TGaxis" [tLine, tAttText] mempty
  [ Constructor [double "xmin", double "ymin", double "xmax", double "ymax", double "wmin", double "wmax", int "ndiv", cstring "chopt", double "gridlength" ] 
  ]


tGraphPolar :: Class
tGraphPolar = 
  grafclass "TGraphPolar" [tGraphErrors] mempty 
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] 
  ]

tGraphQQ :: Class
tGraphQQ = 
  grafclass "TGraphQQ" [tGraph] mempty
  [ Constructor [int "nx", doublep "x", int "ny", doublep "y"] 
  ]


----------------
-- starting L --
----------------

tLine :: Class
tLine = 
  grafclass "TLine" [tObject, tAttLine] mempty
  [ Constructor [double "x1", double "y1", double "x2", double "y2" ] 
  , Virtual (cppclass_ tLine) "DrawLine" [double "x1", double "y1", double "x2", double "y2"]
  , Virtual (cppclass_ tLine) "DrawLineNDC" [double "x1", double "y1", double "x2", double "y2"]
  , NonVirtual double_ "GetX1" [] 
  , NonVirtual double_ "GetX2" [] 
  , NonVirtual double_ "GetY1" [] 
  , NonVirtual double_ "GetY2" [] 
  , NonVirtual bool_ "IsHorizontal" [] 
  , NonVirtual bool_ "IsVertical" [] 
  , Virtual void_ "PaintLine" [double "x1", double "y1", double "x2", double "y2"] 
  , Virtual void_ "PaintLineNDC" [double "u1", double "v1", double "u2", double "v2"] 
  , NonVirtual void_ "SetHorizontal" [bool "set"] 
  , NonVirtual void_ "SetVertical" [bool "set"] 
  , Virtual void_ "SetX1" [double "x1"]
  , Virtual void_ "SetX2" [double "x2"]
  , Virtual void_ "SetY1" [double "y1"]
  , Virtual void_ "SetY2" [double "y2"]
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
  [ Constructor [cstring "name", cstring "title", cstring "material", float "phi1", float "dphi1", int "nz"]
  ]

----------------
-- starting S --
----------------

tShape :: Class 
tShape = 
  grafclass "TShape" [tNamed, tAttLine, tAttFill, tAtt3D] mempty
  [ Constructor [cstring "name", cstring "title", cstring "material" ]  
  ]

tSPHE :: Class
tSPHE = 
  grafclass "TSPHE" [tShape] mempty 
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "themin", float "themax", float "phimin", float "phimax" ]
  ]

----------------
-- starting T --
----------------

tTUBE :: Class
tTUBE = 
  grafclass "TTUBE" [tShape] mempty
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "dz", float "aspect"] 
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

