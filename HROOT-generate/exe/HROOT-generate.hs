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
import           Data.Monoid             ( mempty )
import           Data.List               ( intercalate )
import           Data.Text               ( Text )
import qualified Data.Text as T
import           Data.Typeable           ( Typeable )
import           System.Console.CmdArgs  ( cmdArgs, modes )
import           System.Directory        ( createDirectoryIfMissing, getCurrentDirectory )
import           System.FilePath         ( (</>), (<.>) )
import           System.IO               ( IOMode(..), hPutStrLn, withFile )
--
import           FFICXX.Generate.Builder ( copyFileWithMD5Check, simpleBuilder )
import           FFICXX.Generate.Code.Cabal
                                         ( cabalIndentation
                                         , cabalTemplate
                                         , genExposedModules
                                         , genOtherModules
                                         , unlinesWithIndent
                                         )
import           FFICXX.Generate.Config  ( FFICXXConfig(..)
                                         , SimpleBuilderConfig(..)
                                         )
import           FFICXX.Generate.Dependency ()
import           FFICXX.Generate.ContentMaker ()
import           FFICXX.Generate.Type.Cabal ( CabalName(..) )
import           FFICXX.Generate.Type.Config ( ModuleUnitMap(..) )
import           FFICXX.Generate.Type.Class ()
import           FFICXX.Generate.Type.Module ()
import           FFICXX.Generate.Type.PackageInterface ()
import           FFICXX.Generate.Util              ( conn
                                                   , connRet
                                                   , context
                                                   , contextT
                                                   , intercalateWith
                                                   , subst
                                                   )
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
import           HROOT.Data.RooFit.Class           ( roofitcabal
                                                   , roofit_classes
                                                   , roofit_extraLib
                                                   , roofit_extraDep
                                                   , roofit_headers
                                                   , roofit_topfunctions
                                                   )
import           HROOT.Data.RooFit.RooStats.Class  ( roostatscabal
                                                   , roostats_classes
                                                   , roostats_extraLib
                                                   , roostats_extraDep
                                                   , roostats_headers
                                                   , roostats_topfunctions
                                                   )
import           HROOT.Data.Tree.Class             ( treecabal
                                                   , tree_classes
                                                   , tree_extraLib
                                                   , tree_extraDep
                                                   , tree_headers
                                                   , tree_topfunctions
                                                   )
import qualified Paths_HROOT_generate as H

---------------------------------
-- for umbrella package
---------------------------------

pkgHsTemplate :: Text
pkgHsTemplate =
  "module $summarymod (\n\
  \$exportList\n\
  \) where\n\
  \\n\
  \$importList\n\
  \\n\
  \$topLevelDef\n"

makeUmbrellaCabal :: String
makeUmbrellaCabal =
  let pkgname = "HROOT"
      -- TODO: this should be factored out.
      version = "0.10.0.1"
      pkg_summarymodule = "HROOT"
      pkg_deps = [ "HROOT-core", "HROOT-hist", "HROOT-math"
                 , "HROOT-tree", "HROOT-graf","HROOT-io"
                 ]
      pkg_synopsis = "Haskell binding to the ROOT data analysis framework"
      pkg_description = "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."
      setupdeps = [ CabalName "Cabal", CabalName "base", CabalName "process" ]
      deps | null pkg_deps = ""
           | otherwise     = "base, " ++ intercalate ", " pkg_deps
      str = subst cabalTemplate . contextT $
              [ ("pkgname", pkgname)
              , ("version", version)
              , ("synopsis", pkg_synopsis )
              , ("description", pkg_description )
              , ("homepage", "http://ianwookim.org/HROOT")
              , ("licenseField", "license: LGPL-2.1" )
              , ("licenseFileField", "license-file: LICENSE")
              , ("author", "Ian-Woo Kim")
              , ("maintainer", "Ian-Woo Kim <ianwookim@gmail.com>")
              , ("category", "Graphics, Statistics, Math, Numerical")
              , ("sourcerepository","")
              , ("buildtype", "Build-Type: Custom\ncustom-setup\n  setup-depends: "
                                  <> T.pack (intercalate ", " (map unCabalName setupdeps))
                                  <> "\n")
              , ("cxxOptions", "-std=c++14")
              , ("pkgdeps", T.pack deps)
              , ("extraFiles", cabalIndentation <> "Config.hs" )
              , ("csrcFiles", "")
              , ("includeFiles", "")
              , ("cppFiles", "")
              , ("exposedModules", unlinesWithIndent $ map T.pack $ genExposedModules pkg_summarymodule ([],[]))
              , ("otherModules"  , unlinesWithIndent $ map T.pack $ genOtherModules [])
              , ("extralibdirs",  "" )  -- this need to be changed
              , ("extraincludedirs", "" )  -- this need to be changed
              , ("extraLibraries", "")
              , ("cabalIndentation", cabalIndentation)
              , ("pkgconfigDepends", "")
              ]
  in str

-- | make an umbrella package for this project
makeUmbrellaPackage :: FFICXXConfig -> [String] -> IO ()
makeUmbrellaPackage config mods = do
  let pkgname = "HROOT"
      pkg_summarymodule = "HROOT"

  putStrLn "======================"
  putStrLn "Umbrella Package 'HROOT' generation"
  putStrLn "----------------------"

  let cabalFileName = pkgname <> ".cabal"
      installDir    = fficxxconfig_installBaseDir config
      workingDir    = fficxxconfig_workingDir     config
      staticDir     = fficxxconfig_staticFileDir  config
      staticFiles   = ["CHANGES","Config.hs","LICENSE","README.md","Setup.lhs"]
  putStrLn "cabal file generation"
  --
  createDirectoryIfMissing True installDir
  createDirectoryIfMissing True workingDir
  createDirectoryIfMissing True (installDir </> "src")
  createDirectoryIfMissing True (installDir </> "csrc")
  --
  putStrLn "Copying static files"
  mapM_ (\x->copyFileWithMD5Check (staticDir </> x) (installDir </> x)) staticFiles

  withFile (workingDir </> cabalFileName) WriteMode $ \h ->
    hPutStrLn h makeUmbrellaCabal

  putStrLn "umbrella module generation"
  withFile (workingDir </> pkg_summarymodule <.> "hs" )  WriteMode $ \h -> do
    let exportListStr = intercalateWith (conn "\n, ") (\x->"module " ++ x ) mods
        importListStr = intercalateWith connRet (\x->"import " ++ x) mods
        str = subst pkgHsTemplate . contextT $
                [ ("summarymod", T.pack pkg_summarymodule)
                , ("exportList", T.pack exportListStr)
                , ("importList", T.pack importListStr)
                , ("topLevelDef", "")
                ]
    hPutStrLn h str
  putStrLn "copying"
  copyFileWithMD5Check (workingDir </> cabalFileName)  (installDir </> cabalFileName)
  copyFileWithMD5Check (workingDir </> pkg_summarymodule <.> "hs") (installDir </> "src" </> pkg_summarymodule <.> "hs")

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  tmpldir <- H.getDataDir >>= return . (</> "template")
  -- let tmpldir = "../template"

  let mkcfg name = FFICXXConfig {
                     fficxxconfig_workingDir     = cwd </> "tmp" </> name </> "working"
                   , fficxxconfig_installBaseDir = cwd </> name
                   , fficxxconfig_staticFileDir  = tmpldir </> name
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
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
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
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
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
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
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
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
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
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
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
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
                   }
      sbc_roofit = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.RooFit"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList roofit_headers)
                   , sbcCabal      = roofitcabal
                   , sbcClasses    = roofit_classes
                   , sbcTopLevels  = roofit_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = roofit_extraLib
                   , sbcExtraDeps  = roofit_extraDep
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
                   }
      sbc_roostats = SimpleBuilderConfig {
                     sbcTopModule  = "HROOT.RooFit.RooStats"
                   , sbcModUnitMap = ModuleUnitMap (HM.fromList roostats_headers)
                   , sbcCabal      = roostatscabal
                   , sbcClasses    = roostats_classes
                   , sbcTopLevels  = roostats_topfunctions
                   , sbcTemplates  = []
                   , sbcExtraLibs  = roostats_extraLib
                   , sbcExtraDeps  = roostats_extraDep
                   , sbcStaticFiles = ["CHANGES","Config.hs","LICENSE","Setup.lhs"]
                   }

  simpleBuilder (mkcfg "HROOT-core")            sbc_core
  simpleBuilder (mkcfg "HROOT-graf")            sbc_graf
  simpleBuilder (mkcfg "HROOT-hist")            sbc_hist
  simpleBuilder (mkcfg "HROOT-io")              sbc_io
  simpleBuilder (mkcfg "HROOT-math")            sbc_math
  simpleBuilder (mkcfg "HROOT-RooFit")          sbc_roofit
  simpleBuilder (mkcfg "HROOT-RooFit-RooStats") sbc_roostats
  simpleBuilder (mkcfg "HROOT-tree")            sbc_tree

  -- RooFit and RooStats are not part of main HROOT.
  makeUmbrellaPackage (mkcfg "HROOT") [ "HROOT.Core", "HROOT.Hist", "HROOT.Graf", "HROOT.IO", "HROOT.Math", "HROOT.Tree" ]
