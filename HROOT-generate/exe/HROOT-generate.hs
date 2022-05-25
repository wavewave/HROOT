{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}

module Main where

import Control.Applicative ()
import Control.Monad (liftM3)
import Data.Data (Data)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
--
import FFICXX.Generate.Builder (copyFileWithMD5Check, simpleBuilder)
import FFICXX.Generate.Code.Cabal
  ( cabalIndentation,
    cabalTemplate,
    genExposedModules,
    genOtherModules,
    unlinesWithIndent,
  )
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.ContentMaker ()
import FFICXX.Generate.Dependency ()
import FFICXX.Generate.Type.Cabal (CabalName (..))
import FFICXX.Generate.Type.Class ()
import FFICXX.Generate.Type.Config (ModuleUnitMap (..))
import FFICXX.Generate.Type.Module ()
import FFICXX.Generate.Type.PackageInterface ()
import FFICXX.Generate.Util
  ( conn,
    connRet,
    context,
    contextT,
    intercalateWith,
    subst,
  )
--
import HROOT.Data.Core.Class
  ( core_classes,
    core_extraDep,
    core_extraLib,
    core_headers,
    core_topfunctions,
    corecabal,
  )
import HROOT.Data.Graf.Class
  ( graf_classes,
    graf_extraDep,
    graf_extraLib,
    graf_headers,
    graf_topfunctions,
    grafcabal,
  )
import HROOT.Data.Hist.Class
  ( hist_classes,
    hist_extraDep,
    hist_extraLib,
    hist_headers,
    hist_topfunctions,
    histcabal,
  )
import HROOT.Data.IO.Class
  ( io_classes,
    io_extraDep,
    io_extraLib,
    io_headers,
    io_topfunctions,
    iocabal,
  )
import HROOT.Data.Math.Class
  ( math_classes,
    math_extraDep,
    math_extraLib,
    math_headers,
    math_topfunctions,
    mathcabal,
  )
import HROOT.Data.Net.Class
  ( net_classes,
    net_extraDep,
    net_extraLib,
    net_headers,
    net_topfunctions,
    netcabal,
  )
import HROOT.Data.RooFit.Class
  ( roofit_classes,
    roofit_extraDep,
    roofit_extraLib,
    roofit_headers,
    roofit_topfunctions,
    roofitcabal,
  )
import HROOT.Data.RooFit.RooStats.Class
  ( roostats_classes,
    roostats_extraDep,
    roostats_extraLib,
    roostats_headers,
    roostats_topfunctions,
    roostatscabal,
  )
import HROOT.Data.Tree.Class
  ( tree_classes,
    tree_extraDep,
    tree_extraLib,
    tree_headers,
    tree_topfunctions,
    treecabal,
  )
import qualified Paths_HROOT_generate as H
import System.Console.CmdArgs (cmdArgs, modes)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((<.>), (</>))
import System.IO (IOMode (..), hPutStrLn, withFile)

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
      pkg_deps =
        [ "HROOT-core",
          "HROOT-graf",
          "HROOT-hist",
          "HROOT-io",
          "HROOT-math",
          "HROOT-net",
          "HROOT-tree"
        ]
      pkg_synopsis = "Haskell binding to the ROOT data analysis framework"
      pkg_description = "HROOT is a haskell Foreign Function Interface (FFI) binding to ROOT. ROOT(http://root.cern.ch) is an object-oriented program and library developed by CERN for physics data analysis."
      setupdeps = [CabalName "Cabal", CabalName "base", CabalName "process"]
      deps
        | null pkg_deps = ""
        | otherwise = "base, " ++ intercalate ", " pkg_deps
      str =
        subst cabalTemplate . contextT $
          [ ("pkgname", pkgname),
            ("version", version),
            ("synopsis", pkg_synopsis),
            ("description", pkg_description),
            ("homepage", "http://ianwookim.org/HROOT"),
            ("licenseField", "license: LGPL-2.1-or-later"),
            ("licenseFileField", "license-file: LICENSE"),
            ("author", "Ian-Woo Kim"),
            ("maintainer", "Ian-Woo Kim <ianwookim@gmail.com>"),
            ("category", "Graphics, Statistics, Math, Numerical"),
            ("sourcerepository", ""),
            ( "buildtype",
              "Build-Type: Custom\ncustom-setup\n  setup-depends: "
                <> T.pack (intercalate ", " (map unCabalName setupdeps))
                <> "\n"
            ),
            ("cxxOptions", "-std=c++17"),
            ("pkgdeps", T.pack deps),
            ("extraFiles", cabalIndentation <> "Config.hs"),
            ("csrcFiles", ""),
            ("includeFiles", ""),
            ("cppFiles", ""),
            ("exposedModules", cabalIndentation <> "HROOT"),
            ("otherModules", ""),
            ("extralibdirs", ""), -- this need to be changed
            ("extraincludedirs", ""), -- this need to be changed
            ("extraLibraries", ""),
            ("cabalIndentation", cabalIndentation),
            ("pkgconfigDepends", "")
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
      installDir = fficxxconfig_installBaseDir config
      workingDir = fficxxconfig_workingDir config
      staticDir = fficxxconfig_staticFileDir config
      staticFiles = ["CHANGES", "Config.hs", "LICENSE", "README.md", "Setup.lhs"]
  putStrLn "cabal file generation"
  --
  createDirectoryIfMissing True installDir
  createDirectoryIfMissing True workingDir
  createDirectoryIfMissing True (installDir </> "src")
  createDirectoryIfMissing True (installDir </> "csrc")
  --
  putStrLn "Copying static files"
  mapM_ (\x -> copyFileWithMD5Check (staticDir </> x) (installDir </> x)) staticFiles

  withFile (workingDir </> cabalFileName) WriteMode $ \h ->
    hPutStrLn h makeUmbrellaCabal

  putStrLn "umbrella module generation"
  withFile (workingDir </> pkg_summarymodule <.> "hs") WriteMode $ \h -> do
    let exportListStr = intercalateWith (conn "\n, ") (\x -> "module " ++ x) mods
        importListStr = intercalateWith connRet (\x -> "import " ++ x) mods
        str =
          subst pkgHsTemplate . contextT $
            [ ("summarymod", T.pack pkg_summarymodule),
              ("exportList", T.pack exportListStr),
              ("importList", T.pack importListStr),
              ("topLevelDef", "")
            ]
    hPutStrLn h str
  putStrLn "copying"
  copyFileWithMD5Check (workingDir </> cabalFileName) (installDir </> cabalFileName)
  copyFileWithMD5Check (workingDir </> pkg_summarymodule <.> "hs") (installDir </> "src" </> pkg_summarymodule <.> "hs")

main :: IO ()
main = do
  args <- getArgs
  cwd <- getCurrentDirectory
  tmpldir <-
    if length args == 1
      then pure (args !! 0)
      else H.getDataDir >>= pure . (</> "template")

  let mkcfg name =
        FFICXXConfig
          { fficxxconfig_workingDir = cwd </> "tmp" </> name </> "working",
            fficxxconfig_installBaseDir = cwd </> name,
            fficxxconfig_staticFileDir = tmpldir </> name
          }
      sbc_core =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.Core",
            sbcModUnitMap = ModuleUnitMap (HM.fromList core_headers),
            sbcCabal = corecabal,
            sbcClasses = core_classes,
            sbcTopLevels = core_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = core_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = core_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_graf =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.Graf",
            sbcModUnitMap = ModuleUnitMap (HM.fromList graf_headers),
            sbcCabal = grafcabal,
            sbcClasses = graf_classes,
            sbcTopLevels = graf_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = graf_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = graf_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_hist =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.Hist",
            sbcModUnitMap = ModuleUnitMap (HM.fromList hist_headers),
            sbcCabal = histcabal,
            sbcClasses = hist_classes,
            sbcTopLevels = hist_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = hist_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = hist_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_io =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.IO",
            sbcModUnitMap = ModuleUnitMap (HM.fromList io_headers),
            sbcCabal = iocabal,
            sbcClasses = io_classes,
            sbcTopLevels = io_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = io_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = io_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_math =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.Math",
            sbcModUnitMap = ModuleUnitMap (HM.fromList math_headers),
            sbcCabal = mathcabal,
            sbcClasses = math_classes,
            sbcTopLevels = math_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = math_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = math_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_net =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.Net",
            sbcModUnitMap = ModuleUnitMap (HM.fromList net_headers),
            sbcCabal = netcabal,
            sbcClasses = net_classes,
            sbcTopLevels = net_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = net_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = net_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_tree =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.Tree",
            sbcModUnitMap = ModuleUnitMap (HM.fromList tree_headers),
            sbcCabal = treecabal,
            sbcClasses = tree_classes,
            sbcTopLevels = tree_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = tree_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = tree_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_roofit =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.RooFit",
            sbcModUnitMap = ModuleUnitMap (HM.fromList roofit_headers),
            sbcCabal = roofitcabal,
            sbcClasses = roofit_classes,
            sbcTopLevels = roofit_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = roofit_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = roofit_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }
      sbc_roostats =
        SimpleBuilderConfig
          { sbcTopModule = "HROOT.RooFit.RooStats",
            sbcModUnitMap = ModuleUnitMap (HM.fromList roostats_headers),
            sbcCabal = roostatscabal,
            sbcClasses = roostats_classes,
            sbcTopLevels = roostats_topfunctions,
            sbcTemplates = [],
            sbcExtraLibs = roostats_extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = roostats_extraDep,
            sbcStaticFiles = ["CHANGES", "Config.hs", "LICENSE", "Setup.lhs"]
          }

  simpleBuilder (mkcfg "HROOT-core") sbc_core
  simpleBuilder (mkcfg "HROOT-graf") sbc_graf
  simpleBuilder (mkcfg "HROOT-hist") sbc_hist
  simpleBuilder (mkcfg "HROOT-io") sbc_io
  simpleBuilder (mkcfg "HROOT-math") sbc_math
  simpleBuilder (mkcfg "HROOT-net") sbc_net
  simpleBuilder (mkcfg "HROOT-RooFit") sbc_roofit
  simpleBuilder (mkcfg "HROOT-RooFit-RooStats") sbc_roostats
  simpleBuilder (mkcfg "HROOT-tree") sbc_tree

  -- RooFit and RooStats are not part of main HROOT.
  makeUmbrellaPackage
    (mkcfg "HROOT")
    [ "HROOT.Core",
      "HROOT.Hist",
      "HROOT.Graf",
      "HROOT.IO",
      "HROOT.Math",
      "HROOT.Net",
      "HROOT.Tree"
    ]
