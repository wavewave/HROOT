-- |
-- Module      : HROOT.Data.Core.Class
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module HROOT.Data.Core.Class where

-- import Bindings.Cxx.Generate.Type.CType
-- import Bindings.Cxx.Generate.Type.Method
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module


corecabal = Cabal { cabal_pkgname = "HROOT-core"
                  , cabal_cheaderprefix = "HROOTCore" 
                  , cabal_moduleprefix = "HROOT.Core" 
                  } 

moduleInterface :: Module
moduleInterface = Module { module_name = "HROOT.Class.Interface"
                         , module_exports = [ "IDeletable" 
                                            , "TObject"
                                            , "ITObject"
                                            , "ITNamed" 
                                            , "TNamed" ]
                         }  

deletable :: Class 
deletable = AbstractClass corecabal "Deletable" [] 
          [ Destructor ]



tObject :: Class
tObject = 
  Class corecabal "TObject" [deletable] 
  [ Constructor [] 
  -- , Virtual int_ "DistancetoPrimitive" [int "px", int "py"]
  , Virtual void_    "Draw"    [cstring "option"] 
  -- , Virtual void_ "ExecuteEvent" [int "event", int "px", int "py"]
  , Virtual (cppclass_ tObject) "FindObject" [cstring "name"]
  , Virtual  cstring_ "GetName" [] 
  , Virtual (cppclass_ tClass) "IsA" [] 
  , Virtual void_ "Paint" [cstring "option"] 
  , AliasVirtual void_ "Print" [cstring "option"] "printObj"
  , Virtual void_    "SaveAs"  [cstring "filename", cstring "option"] 
  , Virtual int_     "Write"   [cstring "name", int "option", int "bufsize" ]

  , Static  bool_    "GetObjectStat" []
  ]


tDictionary :: Class
tDictionary = AbstractClass corecabal "TDictionary" [tNamed]
              [
              ]


tNamed :: Class
tNamed = 
  Class corecabal "TNamed" [tObject] 
  [ Constructor [cstring "name", cstring "title"] 
  , Virtual void_  "SetName"      [cstring "name"]
  , Virtual void_  "SetNameTitle" [cstring "name", cstring "title"]
  , Virtual void_  "SetTitle"     [cstring "name"]  
  ]

tClass :: Class
tClass = Class corecabal "TClass" [tDictionary]
         [
         ]



core_classes :: [Class]
core_classes = 
  [ deletable, tObject, tClass, tDictionary, tNamed ] 

