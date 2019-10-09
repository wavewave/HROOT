{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HROOT.Generate.MakePkg where

import           Control.Applicative                           ()
import           Control.Monad                                 ( forM_, when )
import           Data.List
import qualified Data.Map as M
import           Data.Maybe                                    ()
import           Data.Monoid                                   ( (<>) )
import           Data.Text                                     ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Distribution.Package                          ( pkgVersion )
import           Distribution.PackageDescription        hiding ( exposedModules )
import           Distribution.Verbosity                        ()
import           Distribution.Version                          ()
import           Language.Haskell.Exts.Pretty                  ( prettyPrint )
import           System.Console.CmdArgs                        ()
import           System.Directory                              ( createDirectoryIfMissing
                                                               , doesDirectoryExist
                                                               , getDirectoryContents
                                                               )
import           System.FilePath                               ( (</>), (<.>) )
import           System.IO                                     ( Handle, IOMode(..)
                                                               , hPutStrLn, withFile
                                                               )
--
import           FFICXX.Generate.Code.Cabal                    ( cabalIndentation
                                                               , cabalTemplate
                                                               , genCppFiles
                                                               , genCsrcFiles
                                                               , genExposedModules
                                                               , genIncludeFiles
                                                               , genOtherModules
                                                               )
import           FFICXX.Generate.Code.Cpp                      ()
import           FFICXX.Generate.Dependency                    ()
import           FFICXX.Generate.Config                        ( FFICXXConfig(..) )
import           FFICXX.Generate.ContentMaker                  ( buildCastHs
                                                               , buildDeclHeader
                                                               , buildDefMain
                                                               , buildFFIHsc
                                                               , buildImplementationHs
                                                               , buildInterfaceHs
                                                               , buildInterfaceHSBOOT
                                                               , buildModuleHs
                                                               , buildRawTypeHs
                                                               , buildTopLevelCppDef
                                                               , buildTopLevelHeader
                                                               , buildTopLevelHs
                                                               , buildTypeDeclHeader
                                                               , csrcDir
                                                               , mkGlobal
                                                               , srcDir
                                                               )
import           FFICXX.Generate.Builder                       ( copyCppFiles
                                                               , copyFileWithMD5Check
                                                               , copyModule
                                                               , moduleFileCopy
                                                               )
import           FFICXX.Generate.Type.Annotate                 ( AnnotateMap )
import           FFICXX.Generate.Type.Class                    ( Class(..) )
import           FFICXX.Generate.Type.Module                   ( ClassImportHeader(..)
                                                               , ClassModule(..)
                                                               , TopLevelImportHeader(..)
                                                               , PackageConfig(..)
                                                               )
import           FFICXX.Generate.Type.PackageInterface         ( HeaderName(..)
                                                               , Namespace(..)
                                                               , TypeMacro
                                                               )
import           FFICXX.Generate.Util                          ( conn, connRet, contextT
                                                               , intercalateWith
                                                               , subst
                                                               )
--
import qualified Paths_HROOT_generate                   as H

data UmbrellaPackageConfig = UPkgCfg { upkgname :: Text }

data ComponentConfig  =
  CompCfg {
    comp_pkgname           :: Text
  , comp_summarymodule :: Text
  , comp_typemacro     :: TypeMacro
  , comp_pkgconfig     :: PackageConfig
  -- , comp_classes       :: [Class]
  -- , comp_cihs          :: [ClassImportHeader]
  -- , comp_tih           :: TopLevelImportHeader
  -- , comp_modules       :: [ClassModule]
  -- , comp_annotateMap   :: AnnotateMap
  -- , comp_deps          :: [Text]
  -- , comp_hsbootlst     :: [Text]
  -- , comp_synopsis      :: Text
  -- , comp_description   :: Text
  }



-- |
mkCROOTIncludeHeaders :: ([Namespace],String) -> Class -> ([Namespace],[HeaderName])
mkCROOTIncludeHeaders (nss,str) c =
  case class_name c of
    "Deletable" -> (nss,[])
    "TROOT" -> (nss++[NS "ROOT"],[HdrName (str </> (class_name c) ++ ".h")])
    _ -> (nss,[HdrName (str </> (class_name c) ++ ".h")])

-- |
{-
mkCabalFile :: Bool  -- ^ is umbrella
            -> FFICXXConfig
            -> EachPackageConfig
            -> Handle
            -> IO ()
mkCabalFile isUmbrella config PkgCfg {..} h = do
  version <- getHROOTVersion config
  let deps | null pkg_deps = []
           | otherwise = "," ++ intercalate "," pkg_deps
  let str = subst cabalTemplate . contextT $
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
              , ("buildtype", "Custom")
              , ("ccOptions", "-std=c++14")
              , ("deps", deps)
              , ("extraFiles", cabalIndentation <> "Config.hs" )
              , ("csrcFiles", if isUmbrella then "" else genCsrcFiles (pkg_tih,pkg_modules))
              , ("includeFiles", let cihs = cmCIH =<< pkg_modules
                                 in if isUmbrella then "" else genIncludeFiles pkgname (cihs, []))
              , ("cppFiles", if isUmbrella then "" else genCppFiles (pkg_tih,pkg_modules))
              , ("exposedModules", genExposedModules pkg_summarymodule (pkg_modules,[]))
              , ("otherModules", T.pack $ concat $ genOtherModules pkg_modules)
              , ("extralibdirs",  "" )  -- this need to be changed
              , ("extraincludedirs", "" )  -- this need to be changed
              , ("extraLibraries", "")
              , ("cabalIndentation", cabalIndentation)
              ]
  hPutStrLn h str
-}



-- |
getHROOTVersion :: FFICXXConfig -> IO String
getHROOTVersion conf = do
  let hrootgeneratecabal = fficxxconfig_scriptBaseDir conf </> "HROOT-generate.cabal"
  gdescs <- readPackageDescription normal hrootgeneratecabal
  let vnums = versionBranch . pkgVersion . package . packageDescription $ gdescs
  return $ intercalate "." (map show vnums)

-- |
makePackage :: FFICXXConfig -> EachPackageConfig -> IO ()
makePackage config pkgcfg@(PkgCfg {..}) = do
    let workingDir = fficxxconfig_workingDir config
        ibase = fficxxconfig_installBaseDir config
        cabalFileName = pkgname <> ".cabal"
    --
    TIO.putStrLn "======================"
    TIO.putStrLn ("working on " <> pkgname)
    TIO.putStrLn "----------------------"
    TIO.putStrLn "cabal file generation"
    createDirectoryIfMissing ibase
    createDirectoryIfMissing workingDir

    copyPredefinedFiles pkgname (["CHANGES","Config.hs","LICENSE","Setup.lhs"], ["src","csrc"])   ibase

    buildCabalFile       -- config pkgcfg
      cabal
      topLevelMod
      pkgconfig
      extralibs
      (workingDir </> cabalFileName)

    let cglobal = mkGlobal pkg_classes
    --
    putStrLn "header file generation"
    let gen :: FilePath -> String -> IO ()
        gen file str =
          let path = workingDir </> file in withFile path WriteMode (flip hPutStrLn str)


    gen (pkgname <> "Type.h") (buildTypeDeclHeader pkg_typemacro (map cihClass pkg_cihs))
    mapM_ (\hdr -> gen (unHdrName (cihSelfHeader hdr)) (buildDeclHeader pkg_typemacro pkgname hdr)) pkg_cihs
    gen (tihHeaderFileName pkg_tih <.> "h") (buildTopLevelHeader pkg_typemacro pkgname pkg_tih)
    --
    putStrLn "cpp file generation"
    mapM_ (\hdr -> gen (cihSelfCpp hdr) (buildDefMain hdr)) pkg_cihs
    gen (tihHeaderFileName pkg_tih <.> "cpp") (buildTopLevelCppDef pkg_tih)
    --
    putStrLn "RawType.hs file generation"
    mapM_ (\m -> gen (cmModule m <.> "RawType" <.> "hs") (prettyPrint (buildRawTypeHs m))) pkg_modules
    --
    putStrLn "FFI.hsc file generation"
    mapM_ (\m -> gen (cmModule m <.> "FFI" <.> "hsc") (prettyPrint (buildFFIHsc m))) pkg_modules
    --
    putStrLn "Interface.hs file generation"
    mapM_ (\m -> gen (cmModule m <.> "Interface" <.> "hs") (prettyPrint (buildInterfaceHs pkg_annotateMap m))) pkg_modules
    --
    putStrLn "Cast.hs file generation"
    mapM_ (\m -> gen (cmModule m <.> "Cast" <.> "hs") (prettyPrint (buildCastHs m))) pkg_modules
    --
    putStrLn "Implementation.hs file generation"
    mapM_ (\m -> gen (cmModule m <.> "Implementation" <.> "hs") (prettyPrint (buildImplementationHs pkg_annotateMap m))) pkg_modules
    --
    putStrLn "hs-boot file generation"
    mapM_ (\m -> gen (m <.> "Interface" <.> "hs-boot") (prettyPrint (buildInterfaceHSBOOT m))) pkg_hsbootlst
    --
    putStrLn "module file generation"
    mapM_ (\m -> gen (cmModule m <.> "hs") (prettyPrint (buildModuleHs m))) pkg_modules
    --
    putStrLn "summary module generation generation"
    gen (pkg_summarymodule <.> "hs") (buildTopLevelHs pkg_summarymodule pkg_modules pkg_tih)
    --
    putStrLn "copying"
    copyFileWithMD5Check (workingDir </> cabalFileName)  (ibase </> cabalFileName)
    copyCppFiles workingDir (csrcDir ibase) pkgname (PkgConfig pkg_modules pkg_cihs pkg_tih [] [])
    mapM_ (copyModule workingDir (srcDir ibase)) pkg_modules
    moduleFileCopy workingDir (srcDir ibase) (pkg_summarymodule <.> "hs")

    --
    putStrLn "======================"

