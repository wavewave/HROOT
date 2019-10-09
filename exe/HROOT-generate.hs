{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -w #-}
module Main where

import           Control.Applicative ()
import           Control.Monad           ( liftM3 )
import           Data.Configurator as C
import           Data.Configurator.Types ()
import           Data.Data (Data)
import qualified Data.HashMap.Strict as HM
-- import qualified Data.Map as M
import           Data.Monoid             ( mempty )
import           Data.Typeable           ( Typeable )
import           System.Console.CmdArgs  ( cmdArgs, modes )
import           System.Directory        ( getCurrentDirectory )
import           System.FilePath         ( (</>) )
--
import           FFICXX.Generate.Builder ( simpleBuilder )
import           FFICXX.Generate.Config  ( FFICXXConfig(..)
                                         , SimpleBuilderConfig(..)
                                         )
import           FFICXX.Generate.Dependency ()
import           FFICXX.Generate.ContentMaker ()
import           FFICXX.Generate.Type.Config ( ModuleUnitMap(..) )
import           FFICXX.Generate.Type.Class ()
import           FFICXX.Generate.Type.Module ()
import           FFICXX.Generate.Type.PackageInterface ()
--
import           HROOT.Data.Core.Class             ( corecabal
                                                   , core_classes
                                                   , core_extraDep
                                                   , core_extraLib
                                                   , core_headers
                                                   , core_topfunctions
                                                   )
import           HROOT.Data.Hist.Class             ( histcabal
                                                   , hist_classes
                                                   , hist_extraDep
                                                   , hist_extraLib
                                                   , hist_headers
                                                   , hist_topfunctions
                                                   )
import           HROOT.Data.Graf.Class             ( grafcabal
                                                   , graf_classes
                                                   , graf_extraDep
                                                   , graf_extraLib
                                                   , graf_headers
                                                   , graf_topfunctions
                                                   )
import           HROOT.Data.Math.Class             ( mathcabal
                                                   , math_classes
                                                   , math_extraDep
                                                   , math_extraLib
                                                   , math_headers
                                                   , math_topfunctions
                                                   )
import           HROOT.Data.IO.Class               ( iocabal
                                                   , io_classes
                                                   , io_extraLib
                                                   , io_extraDep
                                                   , io_headers
                                                   , io_topfunctions
                                                   )
import           HROOT.Data.RooFit.Class ()
import           HROOT.Data.RooFit.RooStats.Class ()
import           HROOT.Data.Tree.Class             ( treecabal
                                                   , tree_classes
                                                   , tree_extraLib
                                                   , tree_extraDep
                                                   , tree_headers
                                                   , tree_topfunctions
                                                   )

-- import           HROOT.Generate.MakePkg ()
--
-- import qualified Paths_HROOT_generate as H


-- data HROOTGenerate = Generate { config :: FilePath }
--             deriving (Show,Data,Typeable)

-- generate :: HROOTGenerate
-- generate = Generate { config = "HROOT.conf" }

-- mode :: HROOTGenerate
-- mode = modes [generate]


-- headerMap

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let mkcfg name = FFICXXConfig {
                     fficxxconfig_scriptBaseDir  = cwd </> "tmp" </> name </> "script"
                   , fficxxconfig_workingDir     = cwd </> "tmp" </> name </> "working"
                   , fficxxconfig_installBaseDir = cwd </> name
                   }
      sbc_core   = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.Core"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList core_headers)
                   , sbcCabal      = corecabal
                   , sbcClasses    = core_classes
                   , sbcTopLevels  = core_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = core_extraLib
                   , sbcExtraDeps  = core_extraDep
                   }
      sbc_graf   = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.Graf"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList graf_headers)
                   , sbcCabal      = grafcabal
                   , sbcClasses    = graf_classes
                   , sbcTopLevels  = graf_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = graf_extraLib
                   , sbcExtraDeps  = graf_extraDep
                   }
      sbc_hist   = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.Hist"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList hist_headers)
                   , sbcCabal      = histcabal
                   , sbcClasses    = hist_classes
                   , sbcTopLevels  = hist_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = hist_extraLib
                   , sbcExtraDeps  = hist_extraDep
                   }
      sbc_io       = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.IO"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList io_headers)
                   , sbcCabal      = iocabal
                   , sbcClasses    = io_classes
                   , sbcTopLevels  = io_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = io_extraLib
                   , sbcExtraDeps  = io_extraDep
                   }
      sbc_math   = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.Math"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList math_headers)
                   , sbcCabal      = mathcabal
                   , sbcClasses    = math_classes
                   , sbcTopLevels  = math_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = math_extraLib
                   , sbcExtraDeps  = math_extraDep
                   }
      sbc_tree   = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.Tree"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList tree_headers)
                   , sbcCabal      = treecabal
                   , sbcClasses    = tree_classes
                   , sbcTopLevels  = tree_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = tree_extraLib
                   , sbcExtraDeps  = tree_extraDep
                   }
{-      sbc_roofit = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.RooFit"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList roofit_headers)
                   , sbcCabal      = roofitcabal
                   , sbcClasses    = roofit_classes
                   , sbcTopLevels  = roofit_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = roofit_extraLib
                   , sbcExtraDeps  = roofit_extraDep
                   }
      sbc_roostats = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.RooFit.RooStats"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList roostats_headers)
                   , sbcCabal      = roostatscabal
                   , sbcClasses    = roostats_classes
                   , sbcTopLevels  = roostats_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = roostats_extraLib
                   , sbcExtraDeps  = roostast_extraDep
                   }
     -}


  simpleBuilder (mkcfg "HROOT-core")            sbc_core
  simpleBuilder (mkcfg "HROOT-graf")            sbc_graf
  simpleBuilder (mkcfg "HROOT-hist")            sbc_hist
  simpleBuilder (mkcfg "HROOT-io")              sbc_io
  simpleBuilder (mkcfg "HROOT-math")            sbc_math
  -- simpleBuilder (mkcfg "HROOT-RooFit")          sbc_roofit
  -- simpleBuilder (mkcfg "HROOT-RooFit-RooStats") sbc_roostats
  simpleBuilder (mkcfg "HROOT-tree")            sbc_tree



--   param <- cmdArgs mode
--   putStrLn $ show param
--   commandLineProcess param


{-
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
      print
        (config1,config2,cfgtree,cfggraf,cfgmath,cfgio,cfgRooFit,cfgRooStats,cfgHROOT)

-}
      {-
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

-}

{-
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


-}
