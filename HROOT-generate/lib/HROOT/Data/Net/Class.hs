{-# LANGUAGE OverloadedStrings #-}

module HROOT.Data.Net.Class where

import FFICXX.Generate.Code.Primitive ( bool, cstring, void_ )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      , modImports
                                      )
--
import HROOT.Data.Core.Class          ( tNamed )


netcabal :: Cabal
netcabal = Cabal {
    cabal_pkgname            = CabalName "HROOT-net"
  , cabal_version            = "0.10.0.1"
  , cabal_cheaderprefix      = "HROOTNet"
  , cabal_moduleprefix       = "HROOT.Net"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "stdcxx", CabalName "HROOT-core", CabalName "HROOT-io" ]
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Custom [CabalName "Cabal", CabalName "base", CabalName "process"]
  }

netclass :: String -> [Class] -> [Function] -> Class
netclass n ps fs =
  Class {
      class_cabal      = netcabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    , class_has_proxy  = False
    }

tHttpServer :: Class
tHttpServer =
  netclass "THttpServer" [tNamed]
  [ Constructor [cstring "engine"] Nothing
  , NonVirtual void_ "SetReadOnly" [bool "readonly"] Nothing
  ]

net_classes :: [Class]
net_classes =
  [ tHttpServer ]

net_topfunctions :: [TopLevelFunction]
net_topfunctions = []

net_headers :: [(ModuleUnit,ModuleUnitImports)]
net_headers =
  [ modImports "THttpServer" ["ROOT"] ["THttpServer.h"]
  ]

net_extraLib :: [String]
net_extraLib = [ "RHTTP" ]

net_extraDep :: [(String,[String])]
net_extraDep = []
