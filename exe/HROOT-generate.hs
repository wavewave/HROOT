{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Executable  : HROOT-generate
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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
import           Bindings.Cxx.Generate.Type.Module
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
-- 
import           HROOT.Data.RooFit.Class 
import           HROOT.Data.RooFit.RooStats.Class

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
          mkAllClassModulesAndCIH (name,mkCROOTIncludeHeaders ([],"")) cs
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
pkg_GRAF = mkPkgCfg "HROOT-graf" "HROOT.Graf" "__HROOT_GRAF__" ["HROOT-core","HROOT-hist"] graf_classes
pkg_HIST = mkPkgCfg "HROOT-hist" "HROOT.Hist" "__HROOT_HIST__" ["HROOT-core"] hist_classes
pkg_MATH = mkPkgCfg "HROOT-math" "HROOT.Math" "__HROOT_MATH__" ["HROOT-core"] math_classes
pkg_IO   = mkPkgCfg "HROOT-io"   "HROOT.IO"   "__HROOT_IO__"   ["HROOT-core"] io_classes
pkg_RooFit = mkPkgCfg "HROOT-RooFit" "HROOT.RooFit" "__HROOT_ROOFIT__" ["HROOT-core", "HROOT-math"] roofit_classes
{-    let (mods,cihs) = 
          mkAllClassModulesAndCIH ( "HROOT-RooFit"
                                  , mkCROOTIncludeHeaders ([],"RooStats")) roofit_classes
    in PkgCfg { pkgname = "HROOT-RooFit"
              , pkg_summarymodule = "HROOT.RooFit"
              , pkg_typemacro = "__HROOT_ROOFIT__"
              , pkg_classes = roofit_classes
              , pkg_cihs = cihs 
              , pkg_modules = mods 
              , pkg_annotateMap = M.empty  -- for the time being 
              , pkg_deps = [ "HROOT-core" ]
              } -}
pkg_RooStats = -- mkPkgCfg "HROOT-RooFit" "HROOT.RooFit" "__HROOT_ROOFIT__" ["HROOT-core"] roofit_classes
    let (mods,cihs) = 
          mkAllClassModulesAndCIH ( "HROOT-RooFit-RooStats"
                                  , mkCROOTIncludeHeaders ([NS "RooStats"],"RooStats")) roostats_classes
    in PkgCfg { pkgname = "HROOT-RooFit-RooStats"
              , pkg_summarymodule = "HROOT.RooFit.RooStats"
              , pkg_typemacro = "__HROOT_ROOFIT_ROOSTATS__"
              , pkg_classes = roostats_classes
              , pkg_cihs = cihs 
              , pkg_modules = mods 
              , pkg_annotateMap = M.empty  -- for the time being 
              , pkg_deps = [ "HROOT-core" 
                           , "HROOT-math"
                           , "HROOT-RooFit"
                           ]
              }



pkg_HROOT = PkgCfg { pkgname = "HROOT" 
                   , pkg_summarymodule = "HROOT"
                   , pkg_typemacro = ""
                   , pkg_classes = [] 
                   , pkg_cihs = [] 
                   , pkg_modules = [] 
                   , pkg_annotateMap = M.empty  -- for the time being 
                   , pkg_deps = ["HROOT-core","HROOT-graf","HROOT-hist","HROOT-math","HROOT-io" ]
                   }


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
  mRooFit <- liftM3 FFICXXConfig 
             <$> C.lookup cfg "HROOT-RooFit.scriptbase"
             <*> C.lookup cfg "HROOT-RooFit.workingdir"
             <*> C.lookup cfg "HROOT-RooFit.installbase"
  mRooStats <- liftM3 FFICXXConfig 
               <$> C.lookup cfg "HROOT-RooFit-RooStats.scriptbase"
               <*> C.lookup cfg "HROOT-RooFit-RooStats.workingdir"
               <*> C.lookup cfg "HROOT-RooFit-RooStats.installbase"



  mHROOT <- liftM3 FFICXXConfig 
            <$> C.lookup cfg "HROOT.scriptbase"
            <*> C.lookup cfg "HROOT.workingdir"
            <*> C.lookup cfg "HROOT.installbase"
  

  let mcfg = (,,,,,,,) 
             <$> mfficxxcfg1 
             <*> mfficxxcfg2 
             <*> mgraf 
             <*> mmath 
             <*> mio 
             <*> mRooFit
             <*> mRooStats
             <*> mHROOT
  case mcfg of 
    Nothing -> error "config file is not parsed well"
    Just (config1,config2,cfggraf,cfgmath,cfgio,cfgRooFit,cfgRooStats,cfgHROOT) -> do 
      makePackage config1 pkg_CORE
      makePackage config2 pkg_HIST
      makePackage cfggraf pkg_GRAF
      makePackage cfgmath pkg_MATH
      makePackage cfgio   pkg_IO
      makePackage cfgRooFit pkg_RooFit
      makePackage cfgRooStats pkg_RooStats
    
      makeUmbrellaPackage cfgHROOT pkg_HROOT [ "HROOT.Core" 
                                             , "HROOT.Hist"  
                                             , "HROOT.Graf"  
                                             , "HROOT.IO"    
                                             , "HROOT.Math" ] 


