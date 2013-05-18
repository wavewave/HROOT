{-# LANGUAGE QuasiQuotes #-}

module HROOT.Data.RooFit.Annotate where

import Bindings.Cxx.Generate.Type.Annotate 
import Bindings.Cxx.Generate.QQ.Verbatim


import qualified Data.Map as M


core_ann :: AnnotateMap 
core_ann = M.fromList [] 
