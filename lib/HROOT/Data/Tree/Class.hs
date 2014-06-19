-- |
-- Module      : HROOT.Data.Tree.Class
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
-- 
-- License     : LGPL-2.1
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Tree.Class where

import Data.Monoid
-- 
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
--
import HROOT.Data.Core.Class

treecabal = Cabal { cabal_pkgname = "HROOT-tree"
                  , cabal_cheaderprefix = "HROOTTree" 
                  , cabal_moduleprefix = "HROOT.Tree" 
                  } 

treeclass n ps ann fs = Class treecabal n ps ann Nothing fs 

tBranch :: Class
tBranch = 
  treeclass "TBranch" [tNamed, tAttFill] mempty
  [ 
  ] 

tTree :: Class
tTree = 
  treeclass "TTree" [tNamed,tAttLine,tAttFill,tAttMarker] mempty 
  [ Constructor [cstring "name", cstring "title", int "splitlevel" ] Nothing
  , Virtual int_ "Branch" [ cstring "folder", int "bufsize", int "splitlevel" ] Nothing
  , Virtual (cppclass_ tBranch) "Branch" [ cstring "name", voidp "address", cstring "leaflist", int "bufsize" ] (Just "branch1")
  , Virtual int_ "Fill" [ ] (Just "fillTree")
  ]

tree_classes :: [Class] 
tree_classes = 
  [ tTree, tBranch 
  ] 

tree_topfunctions = 
  [ 
  ] 






