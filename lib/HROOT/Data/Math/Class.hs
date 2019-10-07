module HROOT.Data.Math.Class where

import FFICXX.Generate.Code.Primitive ( double, double_, int, int_, void_ )
import FFICXX.Generate.Type.Cabal     ( Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import HROOT.Data.Core.Class          ( tNamed )


mathcabal :: Cabal
mathcabal =
  Cabal {
    cabal_pkgname       = CabalName "HROOT-math"
  , cabal_version       = "0.10.0.1"
  , cabal_cheaderprefix = "HROOTMath"
  , cabal_moduleprefix  = "HROOT.Math"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  }


mathclass :: String -> [Class] -> [Function] -> Class
mathclass n ps fs =
  Class {
      class_cabal      = mathcabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    }

tRandom :: Class
tRandom =
  mathclass "TRandom" [tNamed]
  [ Constructor [ int "seed" ] Nothing
  , Virtual int_ "GetSeed" [] Nothing
  , Virtual double_ "Gaus" [double "mean", double "sigma"] Nothing
  , Virtual void_ "SetSeed" [ int "seed" ] Nothing
  , Virtual double_ "Uniform" [double "x1", double "x2"] Nothing
  ]

-- rootFitFitResult :: Class
-- rootFitFitResult =
--  mathclass "ROOT::Fit::FitResult" [] mempty
--  [ ]

math_classes :: [Class]
math_classes =
  [ tRandom {- , rootFitFitResult -} ]

math_topfunctions :: [TopLevelFunction]
math_topfunctions = []
