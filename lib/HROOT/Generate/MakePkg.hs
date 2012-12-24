{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HROOT.Generate.MakePkg
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

module HROOT.Generate.MakePkg where

import           Control.Applicative
import           Control.Monad
-- import           Data.Configurator as C
-- import           Data.Configurator.Types 
import           Data.List 
import qualified Data.Map as M
import           Data.Maybe
import           Distribution.Package
import           Distribution.PackageDescription hiding (exposedModules)
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           Distribution.Version 
import           System.Console.CmdArgs
import           System.Directory
import           System.FilePath 
import           System.IO
-- import           Text.Parsec
import           Text.StringTemplate hiding (render)
-- 
import           Bindings.Cxx.Generate.Code.Cabal
import           Bindings.Cxx.Generate.Code.Cpp
import           Bindings.Cxx.Generate.Code.Dependency
import           Bindings.Cxx.Generate.Config
import           Bindings.Cxx.Generate.Generator.ContentMaker 
import           Bindings.Cxx.Generate.Generator.Driver
import           Bindings.Cxx.Generate.Type.Annotate
import           Bindings.Cxx.Generate.Type.Class
import           Bindings.Cxx.Generate.Util
-- 
-- import           HROOT.Generate.ROOT
-- import           HROOT.Generate.ROOTAnnotate
-- import           HROOT.Generate.ROOTModule
-- import           HROOT.Data.Core.ROOTsmall
-- import           HROOT.Data.Core.ROOTAnnotatesmall
-- import           HROOT.Data.Core.ROOTModulesmall
-- 
import qualified Paths_HROOT_generate as H
import qualified Paths_fficxx as F


data PackageConfig  = PkgCfg { hprefix :: String 
                             , pkgname :: String 
                             , pkg_classes :: [Class] 
                             , pkg_annotateMap :: AnnotateMap
                             } 

{- 
main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param 
  commandLineProcess param 
-}

cabalTemplate :: String 
cabalTemplate = "Pkg.cabal"

{- 
hprefix :: String 
hprefix = "HROOT"

-- | 
pkgname :: String 
pkgname = "HROOT"
-- cprefix :: String 
-- cprefix = "HROOT"
-}


-- | 
copyPredefinedFiles :: PackageConfig -> FilePath -> IO () 
copyPredefinedFiles PkgCfg {..} ibase = do 
    tmpldir <- H.getDataDir >>= return . (</> "template") 
    mapM_ (\x->copyFile (tmpldir </> pkgname </> x) (ibase </> x))
      [ "CHANGES", "Config.hs", "LICENSE", "README.md", "Setup.lhs" ]
    notExistThenCreate (ibase </> "example") 
    notExistThenCreate (ibase </> "src") 
    notExistThenCreate (ibase </> "csrc")
    contents <- getDirectoryContents (tmpldir </> pkgname </> "example")
    mapM_ (f (tmpldir </> pkgname </> "example") 
          (ibase </> "example")) contents 
  where 
    f src dest s = if s /= "." && s /= ".."
                   then copyFile (src</>s) (dest</>s) 
                   else return () 


-- | 
mkCROOTIncludeHeaders :: Class -> [String] 
mkCROOTIncludeHeaders c = 
  case class_name c of
    "Deletable" -> [] 
    _ -> [(class_name c) ++ ".h"]

-- | 
mkCabalFile :: FFICXXConfig -> PackageConfig -> Handle -> [ClassModule] -> IO () 
mkCabalFile config PkgCfg {..} h classmodules = do 
  version <- getHROOTVersion config
  templateDir <- F.getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 
  let str = renderTemplateGroup 
              templates 
              [ ("pkgname", pkgname) 
              , ("version", version) 
              , ("csrcFiles", genCsrcFiles classmodules)
              , ("includeFiles", genIncludeFiles pkgname classmodules) 
              , ("cppFiles", genCppFiles classmodules)
              , ("exposedModules", genExposedModules hprefix classmodules) 
              , ("otherModules", genOtherModules hprefix classmodules)
              , ("cabalIndentation", cabalIndentation)
              ]
              cabalTemplate 
  hPutStrLn h str

-- | 
getHROOTVersion :: FFICXXConfig -> IO String 
getHROOTVersion conf = do 
  let hrootgeneratecabal = fficxxconfig_scriptBaseDir conf </> "HROOT-generate.cabal"
  gdescs <- readPackageDescription normal hrootgeneratecabal
  let vnums = versionBranch . pkgVersion . package . packageDescription $ gdescs 
  return $ intercalate "." (map show vnums)

-- |
makePackage :: FFICXXConfig -> PackageConfig -> IO () 
makePackage config pkgcfg@(PkgCfg {..}) = do 
    let workingDir = fficxxconfig_workingDir config 
        ibase = fficxxconfig_installBaseDir config
        cabalFileName = pkgname <.> "cabal" -- cabalTemplate -- "HROOT.cabal"
        (pkg_modules,pkg_classes_imports) = 
          mkAllClassModulesAndCIH (pkgname,mkCROOTIncludeHeaders) pkg_classes
    putStrLn "cabal file generation" 
    getHROOTVersion config
    copyPredefinedFiles pkgcfg ibase 
    withFile (workingDir </> cabalFileName) WriteMode $ 
      \h -> do 
        mkCabalFile config pkgcfg h pkg_modules 
    templateDir <- F.getDataDir >>= return . (</> "template")
    (templates :: STGroup String) <- directoryGroup templateDir 
    let cglobal = mkGlobal pkg_classes
    -- 
    putStrLn "header file generation"
    writeTypeDeclHeaders templates cglobal workingDir pkgname pkg_classes_imports
    mapM_ (writeDeclHeaders templates cglobal workingDir pkgname) pkg_classes_imports
    -- 
    putStrLn "cpp file generation" 
    mapM_ (writeCppDef templates workingDir) pkg_classes_imports
    -- 
    putStrLn "RawType.hs file generation" 
    mapM_ (writeRawTypeHs templates workingDir hprefix) pkg_modules 
    -- 
    putStrLn "FFI.hsc file generation"
    mapM_ (writeFFIHsc templates workingDir hprefix) pkg_modules
    -- 
    putStrLn "Interface.hs file generation" 
    mapM_ (writeInterfaceHs pkg_annotateMap templates workingDir hprefix) pkg_modules
    -- 
    putStrLn "Cast.hs file generation"
    mapM_ (writeCastHs templates workingDir hprefix) pkg_modules
    -- 
    putStrLn "Implementation.hs file generation"
    mapM_ (writeImplementationHs pkg_annotateMap templates workingDir hprefix) pkg_modules
    -- 
    putStrLn "module file generation" 
    mapM_ (writeModuleHs templates workingDir hprefix) pkg_modules
    -- 
    putStrLn "HROOT.hs file generation"
    writePkgHs (pkgname,hprefix) templates workingDir pkg_modules
    -- 
    copyFile (workingDir </> cabalFileName)  ( ibase </> cabalFileName ) 
    copyPredefined templateDir (srcDir ibase) pkgname
    mapM_ (copyCppFiles workingDir (csrcDir ibase) pkgname) pkg_classes_imports
    mapM_ (copyModule workingDir (srcDir ibase) hprefix pkgname) pkg_modules 


