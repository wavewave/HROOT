{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Executable  : HROOT-generate
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Generate source code for HROOT  
--
-----------------------------------------------------------------------------

module Main where


import           Control.Applicative
import           Control.Monad
import           Data.Configurator as C
import           Data.Configurator.Types 
import qualified Data.HashMap.Strict as HM
import           System.Console.CmdArgs
-- 
import           Bindings.Cxx.Generate.Config
import           Bindings.Cxx.Generate.Code.Dependency
import           Bindings.Cxx.Generate.Generator.ContentMaker
import           Bindings.Cxx.Generate.Type.Class 
-- import           Bindings.Cxx.Generate.Type.PackageInterface
-- 
import           Command
-- import           HROOT.Data.Core.Annotate
import           HROOT.Data.Core.Class
-- import           HROOT.Data.Hist.Annotate
import           HROOT.Data.Hist.Class 
-- import           HROOT.Data.Graf.Annotate
import           HROOT.Data.Graf.Class
-- import           HROOT.Data.Math.Annotate
import           HROOT.Data.Math.Class
-- import           HROOT.Data.IO.Annotate
import           HROOT.Data.IO.Class

import           HROOT.Generate.MakePkg 
-- 
import qualified Paths_HROOT_generate as H
import qualified Paths_fficxx as F

import qualified Data.Map as M

main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param 
  commandLineProcess param 

mkPkgCfg :: String -> String -> String -> [String] -> [Class] -> PackageConfig 
mkPkgCfg name summary macro deps cs = 
    let (mods,cihs) = 
          mkAllClassModulesAndCIH (name,mkCROOTIncludeHeaders) cs
    in PkgCfg { pkgname = name 
              , pkg_summarymodule = summary
              , pkg_typemacro = macro 
              , pkg_classes = cs 
              , pkg_cihs = cihs 
              , pkg_modules = mods 
              , pkg_annotateMap = M.empty  -- for the time being 
              , pkg_deps = deps
              }

pkg_CORE = mkPkgCfg "HROOT-core" "HROOT.Core" "__HROOT_CORE__" [] core_classes
pkg_GRAF = mkPkgCfg "HROOT-graf" "HROOT.Graf" "__HROOT_GRAF__" ["HROOT-core"] graf_classes
pkg_HIST = mkPkgCfg "HROOT-hist" "HROOT.Hist" "__HROOT_HIST__" ["HROOT-core"] hist_classes
pkg_MATH = mkPkgCfg "HROOT-math" "HROOT.Math" "__HROOT_MATH__" ["HROOT-core"] math_classes
pkg_IO   = mkPkgCfg "HROOT-io"   "HROOT.IO"   "__HROOT_IO__"   ["HROOT-core"] io_classes

commandLineProcess :: HROOTGenerate -> IO () 
commandLineProcess (Generate conf) = do 
  putStrLn "Automatic HROOT binding generation" 
  cfg <- load [Required conf] 
  mfficxxcfg1 <- liftM3 FFICXXConfig  
                 <$> C.lookup cfg "HROOT-core.scriptbase" 
                 <*> C.lookup cfg "HROOT-core.workingdir"
                 <*> C.lookup cfg "HROOT-core.installbase"  
  mfficxxcfg2 <- liftM3 FFICXXConfig 
                 <$> C.lookup cfg "HROOT-hist.scriptbase" 
                 <*> C.lookup cfg "HROOT-hist.workingdir"
                 <*> C.lookup cfg "HROOT-hist.installbase"
  mgraf <- liftM3 FFICXXConfig 
           <$> C.lookup cfg "HROOT-graf.scriptbase" 
           <*> C.lookup cfg "HROOT-graf.workingdir"
           <*> C.lookup cfg "HROOT-graf.installbase"
  mmath <- liftM3 FFICXXConfig 
           <$> C.lookup cfg "HROOT-math.scriptbase" 
           <*> C.lookup cfg "HROOT-math.workingdir"
           <*> C.lookup cfg "HROOT-math.installbase"
  mio   <- liftM3 FFICXXConfig 
           <$> C.lookup cfg "HROOT-io.scriptbase" 
           <*> C.lookup cfg "HROOT-io.workingdir"
           <*> C.lookup cfg "HROOT-io.installbase"

  let mcfg = (,,,,) <$> mfficxxcfg1 <*> mfficxxcfg2 <*> mgraf <*> mmath <*> mio
  case mcfg of 
    Nothing -> error "config file is not parsed well"
    Just (config1,config2,cfggraf,cfgmath,cfgio) -> do 
      makePackage config1 pkg_CORE
      makePackage config2 pkg_HIST
      makePackage cfggraf pkg_GRAF
      makePackage cfgmath pkg_MATH
      makePackage cfgio   pkg_IO
      -- let pinfc = mkPackageInterface HM.empty (PkgName "HROOT-core") core_cihs 
      

      -- print pinfc 

{-      let (core_modules,core_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-core",mkCROOTIncludeHeaders) core_classes
          (hist_modules,hist_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-hist",mkCROOTIncludeHeaders) hist_classes
          (graf_modules,graf_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-graf",mkCROOTIncludeHeaders) graf_classes 
          (math_modules,math_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-math",mkCROOTIncludeHeaders) math_classes 

 
      let pkg_HROOT_CORE = PkgCfg { pkgname = "HROOT-core"
                                  , pkg_summarymodule = "HROOT.Core"
                                  , pkg_typemacro = "__HROOT_CORE__"
                                  , pkg_classes = core_classes 
                                  , pkg_cihs = core_cihs 
                                  , pkg_modules = core_modules 
                                  , pkg_annotateMap = core_ann
                                  , pkg_deps = []
                                  }
          pkg_HROOT_HIST = PkgCfg { pkgname = "HROOT-hist"
                                  , pkg_summarymodule = "HROOT.Hist"
                                  , pkg_typemacro = "__HROOT_HIST__"
                                  , pkg_classes = hist_classes 
                                  , pkg_cihs = hist_cihs 
                                  , pkg_modules = hist_modules 
                                  , pkg_annotateMap = hist_ann
                                  , pkg_deps = [ "HROOT-core" ]
                                  } 
          pkg_HROOT_GRAF = PkgCfg { pkgname = "HROOT-graf"
                                  , pkg_summarymodule = "HROOT.Graf"
                                  , pkg_typemacro = "__HROOT_GRAF__"
                                  , pkg_classes = graf_classes 
                                  , pkg_cihs = graf_cihs 
                                  , pkg_modules = graf_modules 
                                  , pkg_annotateMap = graf_ann
                                  , pkg_deps = [ "HROOT-core" ]
                                  } 
          pkg_HROOT_MATH = PkgCfg { pkgname = "HROOT-math"
                                  , pkg_summarymodule = "HROOT.Math"
                                  , pkg_typemacro = "__HROOT_MATH__"
                                  , pkg_classes = math_classes 
                                  , pkg_cihs = math_cihs 
                                  , pkg_modules = math_modules 
                                  , pkg_annotateMap = math_ann
                                  , pkg_deps = [ "HROOT-core" ]
                                  }  -}
