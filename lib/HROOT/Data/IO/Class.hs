module HROOT.Data.IO.Class where

import FFICXX.Generate.Code.Primitive ( cstring, int )
import FFICXX.Generate.Type.Cabal     ( Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import HROOT.Data.Core.Class          ( tDirectory )


iocabal :: Cabal
iocabal = Cabal {
    cabal_pkgname            = CabalName "HROOT-io"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTIO"
  , cabal_moduleprefix       = "HROOT.IO"
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

ioclass :: String -> [Class] -> [Function] -> Class
ioclass n ps fs =
  Class {
      class_cabal      = iocabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    }

tDirectoryFile :: Class
tDirectoryFile =
  ioclass "TDirectoryFile" [tDirectory]
  [ {-  Virtual (cppclass_ "TList") "GetListOfKeys" []  -}
  ]

tFile :: Class
tFile = ioclass "TFile" [tDirectoryFile]
        [ Constructor [cstring "fname", cstring "option", cstring "ftitle", int "compress" ] Nothing
        ]


io_classes :: [Class]
io_classes =
  [ tDirectoryFile, tFile ]

io_topfunctions :: [TopLevelFunction]
io_topfunctions = []
