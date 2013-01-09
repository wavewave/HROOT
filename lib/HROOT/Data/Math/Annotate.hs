{-# LANGUAGE QuasiQuotes #-}

module HROOT.Data.Math.Annotate where

import Bindings.Cxx.Generate.Type.Annotate 
import Bindings.Cxx.Generate.QQ.Verbatim


import qualified Data.Map as M


math_ann :: AnnotateMap 
math_ann = M.empty
