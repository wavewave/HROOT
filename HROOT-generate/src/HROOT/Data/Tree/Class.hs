{-# LANGUAGE OverloadedStrings #-}

module HROOT.Data.Tree.Class where

import FFICXX.Generate.Code.Primitive
  ( cppclass,
    cppclass_,
    cstring,
    int,
    int_,
    long,
    voidp,
  )
import FFICXX.Generate.Type.Cabal (BuildType (..), Cabal (..), CabalName (..))
import FFICXX.Generate.Type.Class
  ( Class (..),
    Function (..),
    ProtectedMethod (..),
    TopLevel (..),
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    modImports,
  )
import HROOT.Data.Core.Class (tAttFill, tAttLine, tAttMarker, tNamed)

treecabal :: Cabal
treecabal =
  Cabal
    { cabal_pkgname = CabalName "HROOT-tree",
      cabal_version = "0.10.0.3",
      cabal_cheaderprefix = "HROOTTree",
      cabal_moduleprefix = "HROOT.Tree",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [CabalName "stdcxx", CabalName "HROOT-core"],
      cabal_license = Nothing,
      cabal_licensefile = Nothing,
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = [],
      cabal_buildType = Custom [CabalName "Cabal", CabalName "base", CabalName "process"]
    }

treeclass :: String -> [Class] -> [Function] -> Class
treeclass n ps fs =
  Class
    { class_cabal = treecabal,
      class_name = n,
      class_parents = ps,
      class_protected = Protected [],
      class_alias = Nothing,
      class_funcs = fs,
      class_vars = [],
      class_tmpl_funcs = [],
      class_has_proxy = False
    }

tBranch :: Class
tBranch =
  treeclass
    "TBranch"
    [tNamed, tAttFill]
    []

tChain :: Class
tChain =
  treeclass
    "TChain"
    [tTree]
    [ Constructor [cstring "name", cstring "title"] Nothing,
      Virtual int_ "Add" [cppclass tChain "chain"] (Just "addChain"),
      Virtual int_ "Add" [cstring "name", long "nentries"] (Just "addChain1")
    ]

tTree :: Class
tTree =
  treeclass
    "TTree"
    [tNamed, tAttLine, tAttFill, tAttMarker]
    [ Constructor [cstring "name", cstring "title", int "splitlevel"] Nothing,
      Virtual int_ "Branch" [cstring "folder", int "bufsize", int "splitlevel"] Nothing,
      Virtual (cppclass_ tBranch) "Branch" [cstring "name", voidp "address", cstring "leaflist", int "bufsize"] (Just "branch1"),
      Virtual int_ "Fill" [] (Just "fillTree")
    ]

tree_classes :: [Class]
tree_classes =
  [ tBranch,
    tChain,
    tTree
  ]

tree_topfunctions :: [TopLevel]
tree_topfunctions =
  []

tree_headers :: [(ModuleUnit, ModuleUnitImports)]
tree_headers =
  [ modImports "TBranch" ["ROOT"] ["TBranch.h"],
    modImports "TChain" ["ROOT"] ["TChain.h"],
    modImports "TTree" ["ROOT"] ["TTree.h"]
  ]

tree_extraLib :: [String]
tree_extraLib = []

tree_extraDep :: [(String, [String])]
tree_extraDep = []
