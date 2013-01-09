{-# LANGUAGE QuasiQuotes #-}

module HROOT.Data.IO.Annotate where

import Bindings.Cxx.Generate.Type.Annotate 
import Bindings.Cxx.Generate.QQ.Verbatim


import qualified Data.Map as M


io_ann :: AnnotateMap 
io_ann = M.empty
