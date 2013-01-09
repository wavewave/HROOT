-- |
-- Module      : HROOT.Data.IO.Class
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- 
-- License     : LGPL-2
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.IO.Class where

import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module
-- 
import HROOT.Data.Core.Class

iocabal = Cabal { cabal_pkgname = "HROOT-io"
                  , cabal_cheaderprefix = "HROOTIO" 
                  , cabal_moduleprefix = "HROOT.IO" } 

ioclass = Class iocabal

tDirectoryFile :: Class
tDirectoryFile = 
  ioclass "TDirectoryFile" [tDirectory] 
  [ {-  Virtual (cppclass_ "TList") "GetListOfKeys" []  -}

  ]

tFile :: Class
tFile = ioclass "TFile" [tDirectoryFile] 
        [ Constructor [cstring "fname", cstring "option", cstring "ftitle", int "compress" ] 
        ]


io_classes :: [Class]
io_classes = 
  [ tDirectoryFile, tFile ] 










