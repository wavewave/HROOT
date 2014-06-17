{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HROOT.Generate.MakePkg
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
import           FFICXX.Generate.Code.Cabal
import           FFICXX.Generate.Code.Cpp
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.Config
import           FFICXX.Generate.Generator.ContentMaker 
import           FFICXX.Generate.Generator.Driver
import           FFICXX.Generate.Type.Annotate
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.PackageInterface
import           FFICXX.Generate.Util
-- 
import qualified Paths_HROOT_generate as H
import qualified FFICXX.Paths_fficxx as F

data UmbrellaPackageConfig = UPkgCfg { upkgname :: String } 


data PackageConfig  = PkgCfg { pkgname :: String 
                             , pkg_summarymodule :: String 
                             , pkg_typemacro :: TypeMacro
                             , pkg_classes :: [Class] 
                             , pkg_cihs :: [ClassImportHeader]
                             , pkg_tih :: TopLevelImportHeader
                             , pkg_modules :: [ClassModule]
                             , pkg_annotateMap :: AnnotateMap
                             , pkg_deps :: [String]
                             , pkg_hsbootlst :: [String]
                             , pkg_synopsis :: String
                             , pkg_description :: String
                             } 

-- | 
cabalTemplate :: String 
cabalTemplate = "Pkg.cabal"


-- | 
copyPredefinedFiles :: String   -- ^ package name 
                    -> ([String],[String]) -- ^ files in root dir, directories
                    -> FilePath 
                    -> IO () 
copyPredefinedFiles pkgname (files,dirs) ibase = do 
    tmpldir <- H.getDataDir >>= return . (</> "template") 
    mapM_ (\x->copyFileWithMD5Check (tmpldir </> pkgname </> x) (ibase </> x)) files 
    forM_ dirs $ \dir -> do 
      notExistThenCreate (ibase </> dir) 
      b <- doesDirectoryExist (tmpldir </> pkgname </> dir) 
      when b $ do 
        contents <- getDirectoryContents (tmpldir </> pkgname </> dir)
        mapM_ (f (tmpldir </> pkgname </> dir) (ibase </> dir)) contents 
  where 
    f src dest s = if s /= "." && s /= ".."
                   then copyFileWithMD5Check (src</>s) (dest</>s) 
                   else return () 


-- | 
mkCROOTIncludeHeaders :: ([Namespace],String) -> Class -> ([Namespace],[String])
mkCROOTIncludeHeaders (nss,str) c = 
  case class_name c of
    "Deletable" -> (nss,[])
    "TROOT" -> (nss++[NS "ROOT"],[str </> (class_name c) ++ ".h"])
    _ -> (nss,[str </> (class_name c) ++ ".h"])

-- | 
mkCabalFile :: Bool  -- ^ is umbrella 
            -> FFICXXConfig 
            -> PackageConfig 
            -> Handle 
            -> IO () 
mkCabalFile isUmbrella config PkgCfg {..} h = do 
  version <- getHROOTVersion config
  templateDir <- F.getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 
  let deps | null pkg_deps = [] 
           | otherwise = "," ++ intercalate "," pkg_deps 
  let str = renderTemplateGroup 
              templates 
              [ ("pkgname", pkgname) 
              , ("version", version) 
              , ("synopsis", pkg_synopsis )
              , ("description", pkg_description )
              , ("license", "LGPL-2.1" ) 
              , ("licensefile", "LICENSE")
              , ("buildtype", "Custom")
              , ("deps", deps) 
              , ("csrcFiles", if isUmbrella then "" else genCsrcFiles (pkg_tih,pkg_modules))
              , ("includeFiles", if isUmbrella then "" else genIncludeFiles pkgname pkg_modules)
              , ("cppFiles", if isUmbrella then "" else genCppFiles (pkg_tih,pkg_modules))
              , ("exposedModules", genExposedModules pkg_summarymodule pkg_modules) 
              , ("otherModules", genOtherModules pkg_modules)
              , ("extralibdirs",  "" )  -- this need to be changed 
              , ("extraincludedirs", "" )  -- this need to be changed 
              , ("extralib", "")
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
        cabalFileName = pkgname <.> "cabal" 
    -- 
    putStrLn "======================" 
    putStrLn ("working on " ++ pkgname) 
    putStrLn "----------------------"
    putStrLn "cabal file generation" 
    notExistThenCreate ibase 
    notExistThenCreate workingDir 

    copyPredefinedFiles pkgname (["CHANGES","Config.hs","LICENSE","Setup.lhs"], ["src","csrc"])   ibase 

    withFile (workingDir </> cabalFileName) WriteMode $ 
      \h -> mkCabalFile False config pkgcfg h
    templateDir <- F.getDataDir >>= return . (</> "template")
    (templates :: STGroup String) <- directoryGroup templateDir 
    let cglobal = mkGlobal pkg_classes
    -- 
    putStrLn "header file generation"
    writeTypeDeclHeaders templates workingDir pkg_typemacro pkgname pkg_cihs
    mapM_ (writeDeclHeaders templates workingDir pkg_typemacro pkgname) pkg_cihs
    writeTopLevelFunctionHeaders templates workingDir pkg_typemacro pkgname pkg_tih
    -- 
    putStrLn "cpp file generation" 
    mapM_ (writeCppDef templates workingDir) pkg_cihs
    writeTopLevelFunctionCppDef templates workingDir pkg_typemacro pkgname pkg_tih
    -- 
    putStrLn "RawType.hs file generation" 
    mapM_ (writeRawTypeHs templates workingDir) pkg_modules 
    -- 
    putStrLn "FFI.hsc file generation"
    mapM_ (writeFFIHsc templates workingDir) pkg_modules
    -- 
    putStrLn "Interface.hs file generation" 
    mapM_ (writeInterfaceHs pkg_annotateMap templates workingDir) pkg_modules
    -- 
    putStrLn "Cast.hs file generation"
    mapM_ (writeCastHs templates workingDir) pkg_modules
    -- 
    putStrLn "Implementation.hs file generation"
    mapM_ (writeImplementationHs pkg_annotateMap templates workingDir) pkg_modules
    -- 
    putStrLn "hs-boot file generation" 
    mapM_ (writeInterfaceHSBOOT templates workingDir) pkg_hsbootlst  
    -- 
    putStrLn "module file generation" 
    mapM_ (writeModuleHs templates workingDir) pkg_modules
    -- 
    putStrLn "summary module generation generation"
    writePkgHs pkg_summarymodule templates workingDir pkg_modules pkg_tih
    -- 
    putStrLn "copying"
    copyFileWithMD5Check (workingDir </> cabalFileName)  (ibase </> cabalFileName) 
    copyCppFiles workingDir (csrcDir ibase) pkgname (pkg_tih,pkg_cihs)
    mapM_ (copyModule workingDir (srcDir ibase) pkg_summarymodule) pkg_modules 
    -- 
    putStrLn "======================"

---------------------------------
-- for umbrella package 
---------------------------------

-- | make an umbrella package for this project
makeUmbrellaPackage :: FFICXXConfig -> PackageConfig -> [String] -> IO () 
makeUmbrellaPackage config pkgcfg@(PkgCfg {..}) mods = do 
    putStrLn "======================"
    putStrLn "Umbrella Package 'HROOT' generation"
    putStrLn "----------------------"

    let cabalFileName = pkgname <.> "cabal" 
        ibase = fficxxconfig_installBaseDir config 
        workingDir = fficxxconfig_workingDir config 
    putStrLn "cabal file generation"
    -- 
    notExistThenCreate ibase
    notExistThenCreate workingDir
    -- 
    copyPredefinedFiles pkgname 
      (["CHANGES","Config.hs","LICENSE","README.md","Setup.lhs"],["example","src","csrc"]) ibase 
    withFile (workingDir </> cabalFileName) WriteMode $ 
      \h -> mkCabalFile True config pkgcfg h

    templateDir <- F.getDataDir >>= return . (</> "template")
    (templates :: STGroup String) <- directoryGroup templateDir 

    putStrLn "umbrella module generation"
    withFile (workingDir </> pkg_summarymodule <.> "hs" )  WriteMode $ \h -> do 
      let exportListStr = intercalateWith (conn "\n, ") (\x->"module " ++ x ) mods 
          importListStr = intercalateWith connRet (\x->"import " ++ x) mods
          str = renderTemplateGroup 
                  templates 
                  [ ("summarymod", pkg_summarymodule)
                  , ("exportList", exportListStr) 
                  , ("importList", importListStr) ]
                  "Pkg.hs"
      hPutStrLn h str
    putStrLn "copying"
    copyFileWithMD5Check (workingDir </> cabalFileName)  (ibase </> cabalFileName) 
    copyFileWithMD5Check (workingDir </> pkg_summarymodule <.> "hs") (ibase </> "src" </> pkg_summarymodule <.> "hs")  

