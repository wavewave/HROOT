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
import           HROOT.Data.Core.ROOTsmall
import           HROOT.Data.Core.ROOTAnnotatesmall
import           HROOT.Data.Core.ROOTModulesmall
import           HROOT.Data.Hist.Annotate
import           HROOT.Data.Hist.Class 
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
  case (,) <$> mfficxxcfg1 <*> mfficxxcfg2 of 
    Nothing -> error "config file is not parsed well"
    Just (config1,config2) -> do 
      let -- (core_modules,core_cihs) = 
          --   mkAllClassModulesAndCIH ("HROOT-core",mkCROOTIncludeHeaders) core_classes
          (hist_modules,hist_cihs) = 
            mkAllClassModulesAndCIH ("HROOT-hist",mkCROOTIncludeHeaders) hist_classes
      makePackage config1 (PkgCfg "HROOT.Core" "HROOT-core" core_classes core_cihs core_modules core_ann)
      let pinfc = mkPackageInterface HM.empty (PkgName "HROOT-core") core_cihs 
      
      makePackage config2 (PkgCfg "HROOT.Hist" "HROOT-hist" hist_classes hist_cihs hist_modules hist_ann) 

      print pinfc 

{-
cabalTemplate :: String 
cabalTemplate = "Pkg.cabal"


hprefix :: String 
hprefix = "HROOT"

-- | 
pkgname :: String 
pkgname = "HROOT"
-- cprefix :: String 
-- cprefix = "HROOT"

-- | 
copyPredefinedFiles :: FilePath -> IO () 
copyPredefinedFiles ibase = do 
    tmpldir <- H.getDataDir >>= return . (</> "template") 
    mapM_ (\x->copyFile (tmpldir </> pkgname </> x) (ibase </> x))
      [ "CHANGES", "Config.hs", "LICENSE", "README.md", "Setup.lhs" ]
    notExistThenCreate (ibase </> "example") 
    notExistThenCreate (ibase </> "src") 
    notExistThenCreate (ibase </> "csrc")
    contents <- getDirectoryContents (tmpldir </> pkgname </> "example")
    mapM_ (f (tmpldir </> pkgname </> "example") (ibase </> "example")) contents 
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
mkCabalFile :: FFICXXConfig -> Handle -> [ClassModule] -> IO () 
mkCabalFile config h classmodules = do 
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
commandLineProcess :: HROOTGenerate -> IO () 
commandLineProcess (Generate conf) = do 
  putStrLn "Automatic HROOT binding generation" 
  cfg <- load [Required conf] 
  mfficxxcfg <- liftM3 FFICXXConfig  
                <$> C.lookup cfg "scriptbase" 
                <*> C.lookup cfg "workingdir"
                <*> C.lookup cfg "installbase"  
  case mfficxxcfg of 
    Nothing -> error "config file is not parsed well"
    Just config -> do 
      let workingDir = fficxxconfig_workingDir config 
          ibase = fficxxconfig_installBaseDir config
          cabalFileName = pkgname <.> "cabal" -- cabalTemplate -- "HROOT.cabal"
          (root_all_modules,root_all_classes_imports) = 
            mkAllClassModulesAndCIH (pkgname,mkCROOTIncludeHeaders) root_all_classes
      putStrLn "cabal file generation" 
      getHROOTVersion config
      copyPredefinedFiles ibase 
      withFile (workingDir </> cabalFileName) WriteMode $ 
        \h -> do 
          mkCabalFile config h root_all_modules 
      templateDir <- F.getDataDir >>= return . (</> "template")
      (templates :: STGroup String) <- directoryGroup templateDir 
      let cglobal = mkGlobal root_all_classes
      -- 
      putStrLn "header file generation"
      writeTypeDeclHeaders templates cglobal workingDir pkgname root_all_classes_imports
      mapM_ (writeDeclHeaders templates cglobal workingDir pkgname) root_all_classes_imports
      -- 
      putStrLn "cpp file generation" 
      mapM_ (writeCppDef templates workingDir) root_all_classes_imports
      -- 
      putStrLn "RawType.hs file generation" 
      mapM_ (writeRawTypeHs templates workingDir hprefix) root_all_modules 
      -- 
      putStrLn "FFI.hsc file generation"
      mapM_ (writeFFIHsc templates workingDir hprefix) root_all_modules
      -- 
      putStrLn "Interface.hs file generation" 
      mapM_ (writeInterfaceHs annotateMap templates workingDir hprefix) root_all_modules
      -- 
      putStrLn "Cast.hs file generation"
      mapM_ (writeCastHs templates workingDir hprefix) root_all_modules
      -- 
      putStrLn "Implementation.hs file generation"
      mapM_ (writeImplementationHs annotateMap templates workingDir hprefix) root_all_modules
      -- 
      putStrLn "module file generation" 
      mapM_ (writeModuleHs templates workingDir hprefix) root_all_modules
      -- 
      putStrLn "HROOT.hs file generation"
      writePkgHs (pkgname,hprefix) templates workingDir root_all_modules
      -- 
      copyFile (workingDir </> cabalFileName)  ( ibase </> cabalFileName ) 
      copyPredefined templateDir (srcDir ibase) pkgname
      mapM_ (copyCppFiles workingDir (csrcDir ibase) pkgname) root_all_classes_imports
      mapM_ (copyModule workingDir (srcDir ibase) hprefix pkgname) root_all_modules 


-}