{-# LANGUAGE OverloadedStrings #-}

module HROOT.Data.IO.Class where

import FFICXX.Generate.Code.Primitive ( cstring, int )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
--
import HROOT.Data.Core.Class          ( modImports, tDirectory )


iocabal :: Cabal
iocabal = Cabal {
    cabal_pkgname            = CabalName "HROOT-io"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTIO"
  , cabal_moduleprefix       = "HROOT.IO"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "HROOT-core" ]
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Custom [CabalName "Cabal", CabalName "base", CabalName "process"]
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
tFile =
  ioclass "TFile" [tDirectoryFile]
  [ Constructor [cstring "fname", cstring "option", cstring "ftitle", int "compress" ] Nothing
  ]

tMemFile :: Class
tMemFile =
  ioclass "TMemFile" [tFile]
  [ Constructor [cstring "path", cstring "option", cstring "ftitle", int "compress" ] Nothing
  ]


io_classes :: [Class]
io_classes =
  [ tDirectoryFile
  , tFile
  , tMemFile
  ]

io_topfunctions :: [TopLevelFunction]
io_topfunctions = []

io_headers :: [(ModuleUnit,ModuleUnitImports)]
io_headers =
  [ modImports "TDirectoryFile" ["ROOT"] ["TDirectoryFile.h"]
  , modImports "TFile"          ["ROOT"] ["TFile.h"]
  , modImports "TMemFile"       ["ROOT"] ["TMemFile.h"]
  ]

io_extraLib :: [String]
io_extraLib = []

io_extraDep :: [(String,[String])]
io_extraDep = []
