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
import qualified Paths_HROOT_generate as H
import qualified Paths_fficxx as F

data UmbrellaPackageConfig = UPkgCfg { upkgname :: String } 


data PackageConfig  = PkgCfg { pkgname :: String 
                             , pkg_summarymodule :: String 
                             , pkg_typemacro :: String 
                             , pkg_classes :: [Class] 
                             , pkg_cihs :: [ClassImportHeader]
                             , pkg_modules :: [ClassModule]
                             , pkg_annotateMap :: AnnotateMap
                             , pkg_deps :: [String]
                             } 

-- | 
cabalTemplate :: String 
cabalTemplate = "Pkg.cabal"

{-
-- | utility function
createDirIfNotExist :: FilePath -> IO () 
createDirIfNotExist dir = doesDirectoryExist dir >>= flip unless (createDirectory dir)
-}

-- | 
copyPredefinedFiles :: {- PackageConfig -> -}
                       String 
                    -> FilePath 
                    -> IO () 
copyPredefinedFiles {- PkgCfg {..}  -} pkgname ibase = do 
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
mkCabalFile :: Bool  -- ^ is umbrella 
            -> FFICXXConfig 
            -> PackageConfig 
            -> Handle 
            -> [ClassModule] 
            -> IO () 
mkCabalFile isUmbrella config PkgCfg {..} h classmodules = do 
  version <- getHROOTVersion config
  templateDir <- F.getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 
  let deps | null pkg_deps = [] 
           | otherwise = "," ++ intercalate "," pkg_deps 
  let str = renderTemplateGroup 
              templates 
              [ ("pkgname", pkgname) 
              , ("version", version) 
              , ("deps", deps) 
              , ("csrcFiles", if isUmbrella then "" else genCsrcFiles classmodules)
              , ("includeFiles", if isUmbrella then "" else genIncludeFiles pkgname classmodules) 
              , ("cppFiles", if isUmbrella then "" else genCppFiles classmodules)
              , ("exposedModules", genExposedModules pkg_summarymodule classmodules) 
              , ("otherModules", genOtherModules classmodules)
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
    -- getHROOTVersion config
    copyPredefinedFiles pkgname ibase 
    notExistThenCreate workingDir 
    withFile (workingDir </> cabalFileName) WriteMode $ 
      \h -> mkCabalFile False config pkgcfg h pkg_modules 
    templateDir <- F.getDataDir >>= return . (</> "template")
    (templates :: STGroup String) <- directoryGroup templateDir 
    let cglobal = mkGlobal pkg_classes
    -- 
    putStrLn "header file generation"
    writeTypeDeclHeaders templates pkg_typemacro workingDir pkgname pkg_cihs
    mapM_ (writeDeclHeaders templates cglobal workingDir pkgname) pkg_cihs
    -- 
    putStrLn "cpp file generation" 
    mapM_ (writeCppDef templates workingDir) pkg_cihs
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
    putStrLn "module file generation" 
    mapM_ (writeModuleHs templates workingDir) pkg_modules
    -- 
    putStrLn "summary module generation generation"
    writePkgHs pkg_summarymodule templates workingDir pkg_modules
    -- 
    putStrLn "copying"
    copyFile (workingDir </> cabalFileName)  (ibase </> cabalFileName) 
    copyPredefined templateDir (srcDir ibase) pkgname
    mapM_ (copyCppFiles workingDir (csrcDir ibase) pkgname) pkg_cihs
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
    print ibase
    print cabalFileName 
    -- 
    notExistThenCreate workingDir
    notExistThenCreate (workingDir </> "src")
    -- 
    copyPredefinedFiles pkgname ibase 
    withFile (workingDir </> cabalFileName) WriteMode $ 
      \h -> mkCabalFile True config pkgcfg h [] 

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
    copyFile (workingDir </> cabalFileName)  (ibase </> cabalFileName) 
    copyFile (workingDir </> pkg_summarymodule <.> "hs") (ibase </> "src" </> pkg_summarymodule <.> "hs")  

