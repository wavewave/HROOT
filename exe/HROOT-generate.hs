{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Executable  : HROOT-generate
-- Copyright   : (c) 2011-2017 Ian-Woo Kim
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
import qualified Data.Map as M
import           System.Console.CmdArgs
-- 
import           FFICXX.Generate.Config
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.ContentMaker
import           FFICXX.Generate.Type.Annotate
import           FFICXX.Generate.Type.Class 
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface
-- 
import           Command
import           HROOT.Data.Core.Annotate
import           HROOT.Data.Core.Class
import           HROOT.Data.Hist.Annotate
import           HROOT.Data.Hist.Class 
import           HROOT.Data.Graf.Annotate
import           HROOT.Data.Graf.Class
import           HROOT.Data.Math.Annotate
import           HROOT.Data.Math.Class
import           HROOT.Data.IO.Annotate
import           HROOT.Data.IO.Class
import           HROOT.Data.RooFit.Class 
import           HROOT.Data.RooFit.RooStats.Class
import           HROOT.Data.Tree.Annotate 
import           HROOT.Data.Tree.Class

import           HROOT.Generate.MakePkg 
-- 
import qualified Paths_HROOT_generate as H

main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param 
  commandLineProcess param 

mkPkgCfg :: String -> String -> String -> [String] -> ([Class],[TopLevelFunction]) -> AnnotateMap -> String -> String -> EachPackageConfig 
mkPkgCfg name summary macro deps (cs,fs) amap synopsis descr = 
    let PkgConfig mods cihs tih _ _ = 
          mkPackageConfig (name,mkCROOTIncludeHeaders ([],"")) (cs,fs,[],[])
        hsbootlst = mkHSBOOTCandidateList mods 

    in PkgCfg { pkgname = name 
                , pkg_summarymodule = summary
                , pkg_typemacro = TypMcro macro 
                , pkg_classes = cs 
                , pkg_cihs = cihs 
                , pkg_tih = tih
                , pkg_modules = mods 
                , pkg_annotateMap = amap -- M.empty  -- for the time being 
                , pkg_deps = deps
                , pkg_hsbootlst = hsbootlst 
                , pkg_synopsis = synopsis
                , pkg_description = descr
                }

pkg_CORE = mkPkgCfg "HROOT-core" "HROOT.Core" "__HROOT_CORE__" [] (core_classes,core_topfunctions) core_ann "Haskell binding to ROOT Core modules" "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."
pkg_GRAF = mkPkgCfg "HROOT-graf" "HROOT.Graf" "__HROOT_GRAF__" ["HROOT-core","HROOT-hist"] (graf_classes,graf_topfunctions) graf_ann "Haskell binding to ROOT Graf modules" "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."
pkg_HIST = mkPkgCfg "HROOT-hist" "HROOT.Hist" "__HROOT_HIST__" ["HROOT-core"] (hist_classes,hist_topfunctions) hist_ann "Haskell binding to ROOT Hist modules" "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."
pkg_MATH = mkPkgCfg "HROOT-math" "HROOT.Math" "__HROOT_MATH__" ["HROOT-core"] (math_classes,math_topfunctions) M.empty "Haskell binding to ROOT Math modules" "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."


pkg_IO   = mkPkgCfg "HROOT-io"   "HROOT.IO"   "__HROOT_IO__"   ["HROOT-core"] (io_classes,io_topfunctions) M.empty "Haskell binding to ROOT IO modules" "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."

pkg_RooFit = mkPkgCfg "HROOT-RooFit" "HROOT.RooFit" "__HROOT_ROOFIT__" ["HROOT-core", "HROOT-hist", "HROOT-math"] (roofit_classes,roofit_topfunctions) M.empty "Haskell binding to ROOT RooFit modules" "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."



pkg_RooStats = 
    let PkgConfig mods cihs tih _ _ = 
          mkPackageConfig ( "HROOT-RooFit-RooStats"
                          , mkCROOTIncludeHeaders ([NS "RooStats"],"RooStats"))
                          (roostats_classes,roostats_topfunctions,[],[]) 
        hsbootlst = mkHSBOOTCandidateList mods 
    in PkgCfg { pkgname = "HROOT-RooFit-RooStats"
              , pkg_summarymodule = "HROOT.RooFit.RooStats"
              , pkg_typemacro = TypMcro "__HROOT_ROOFIT_ROOSTATS__"
              , pkg_classes = roostats_classes
              , pkg_cihs = cihs 
              , pkg_tih = tih 
              , pkg_modules = mods 
              , pkg_annotateMap = M.empty  -- for the time being 
              , pkg_deps = [ "HROOT-core" 
                           , "HROOT-math"
                           , "HROOT-RooFit"
                           ]
              , pkg_hsbootlst = hsbootlst 
              , pkg_synopsis = ""
              , pkg_description = ""
              }


pkg_TREE = mkPkgCfg "HROOT-tree" "HROOT.Tree" "__HROOT_TREE__" ["HROOT-core"] (tree_classes,tree_topfunctions) tree_ann "Haskell binding to ROOT Tree modules" "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."



pkg_HROOT = PkgCfg { pkgname = "HROOT" 
                   , pkg_summarymodule = "HROOT"
                   , pkg_typemacro = TypMcro ""
                   , pkg_classes = [] 
                   , pkg_cihs = []
                   , pkg_modules = [] 
                   , pkg_annotateMap = M.empty  -- for the time being 
                   , pkg_deps = [ "HROOT-core", "HROOT-hist", "HROOT-math"
                                , "HROOT-tree", "HROOT-graf","HROOT-io"
                                ]
                   , pkg_hsbootlst = [] 
                   , pkg_synopsis = "Haskell binding to the ROOT data analysis framework" 
                   , pkg_description = "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."
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
  mtree <- liftM3 FFICXXConfig 
           <$> C.lookup cfg "HROOT-tree.scriptbase" 
           <*> C.lookup cfg "HROOT-tree.workingdir"
           <*> C.lookup cfg "HROOT-tree.installbase"
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
  

  let mcfg = (,,,,,,,,) 
             <$> mfficxxcfg1 
             <*> mfficxxcfg2 
             <*> mtree
             <*> mgraf 
             <*> mmath 
             <*> mio 
             <*> mRooFit
             <*> mRooStats
             <*> mHROOT
  case mcfg of 
    Nothing -> error "config file is not parsed well"
    Just (config1,config2,cfgtree,cfggraf,cfgmath,cfgio,cfgRooFit,cfgRooStats,cfgHROOT) -> do 
      makePackage config1 pkg_CORE
      makePackage config2 pkg_HIST
      makePackage cfgtree pkg_TREE
      makePackage cfggraf pkg_GRAF
      makePackage cfgmath pkg_MATH
      makePackage cfgio   pkg_IO
      makePackage cfgRooFit pkg_RooFit
      makePackage cfgRooStats pkg_RooStats

    
      makeUmbrellaPackage cfgHROOT pkg_HROOT [ "HROOT.Core" 
                                             , "HROOT.Hist"  
                                             , "HROOT.Graf"  
                                             , "HROOT.IO"    
                                             , "HROOT.Math" 
                                             , "HROOT.Tree"
                                             ] 


