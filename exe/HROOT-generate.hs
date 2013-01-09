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
import           Bindings.Cxx.Generate.Type.PackageInterface
-- 
import           Command
import           HROOT.Data.Core.Annotate
import           HROOT.Data.Core.Class
import           HROOT.Data.Hist.Annotate
import           HROOT.Data.Hist.Class 
import           HROOT.Data.Graf.Annotate
import           HROOT.Data.Graf.Class
import           HROOT.Generate.MakePkg 
-- 
import qualified Paths_HROOT_generate as H
import qualified Paths_fficxx as F

main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param 
  commandLineProcess param 

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
  mfficxxgraf <- liftM3 FFICXXConfig 
                 <$> C.lookup cfg "HROOT-graf.scriptbase" 
                 <*> C.lookup cfg "HROOT-graf.workingdir"
                 <*> C.lookup cfg "HROOT-graf.installbase"
  case (,,) <$> mfficxxcfg1 <*> mfficxxcfg2 <*> mfficxxgraf of 
    Nothing -> error "config file is not parsed well"
    Just (config1,config2,cfggraf) -> do 
      let (core_modules,core_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-core",mkCROOTIncludeHeaders) core_classes
          (hist_modules,hist_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-hist",mkCROOTIncludeHeaders) hist_classes
          (graf_modules,graf_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-graf",mkCROOTIncludeHeaders) graf_classes 
 
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


      makePackage config1 pkg_HROOT_CORE
      makePackage config2 pkg_HROOT_HIST
      makePackage cfggraf pkg_HROOT_GRAF
      -- let pinfc = mkPackageInterface HM.empty (PkgName "HROOT-core") core_cihs 
      

      -- print pinfc 

