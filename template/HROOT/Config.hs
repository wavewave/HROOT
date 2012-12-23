{-# LANGUAGE ScopedTypeVariables #-}

module Config where
 
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

import System.Exit
import System.Process

config :: LocalBuildInfo -> IO (Maybe HookedBuildInfo)
config bInfo = do 
  (excode, out, err) <- readProcessWithExitCode "root-config" ["--glibs"] ""
  liboptset' <- case excode of 
                  ExitSuccess -> do  
                    return . Just .  mkLibraryOptionSet . words $ out
                  _ -> do 
                    putStrLn $ "root-config failure but I am installing HROOT without ROOT. It will not work. This is only for documentation." 
                    return Nothing              
  (excode2,out2,err2) <- readProcessWithExitCode "root-config" ["--incdir"] ""
  incdir' <- case excode2 of 
               ExitSuccess -> do  
                 return . Just . head . words $ out2
               _ -> do 
                 putStrLn $ "root-config failure but I am installing HROOT without ROOT. It will not work. This is only for documentation." 
                 return Nothing
  let Just lib = library . localPkgDescr $ bInfo
      buildinfo = libBuildInfo lib
  let (r :: Maybe HookedBuildInfo) = case liboptset' of 
            Nothing -> Nothing
            Just liboptset -> 
              case incdir' of 
                Nothing -> Nothing 
                Just incdir -> 
                  let hbi = emptyBuildInfo { extraLibs = extraLibs buildinfo 
                                                         ++ libs liboptset
                                           , extraLibDirs = libdirs liboptset 
                                           , includeDirs = incdir : includeDirs buildinfo
                                           }
                  in Just (Just hbi, []) 
  return r 


data LibraryOptionSet = LibraryOptionSet { 
  libs :: [String], 
  libdirs :: [String], 
  libopts :: [String]
} deriving Show

data LibraryOption = Lib String 
                   | Dir String
                   | Opt String 
                   deriving Show

mkLibraryOptionSet :: [String] -> LibraryOptionSet
mkLibraryOptionSet strs = let opts = libraryOptions strs
                          in  foldr f (LibraryOptionSet [] [] []) opts 
  where f x (LibraryOptionSet l d o) = case x of
                                         Lib st -> LibraryOptionSet (st:l) d o 
                                         Dir st -> LibraryOptionSet l (st:d) o 
                                         Opt st -> LibraryOptionSet l d (st:o) 

libraryOptions :: [String] -> [LibraryOption] -- LibraryOptionSet 
libraryOptions = map f 
  where f x = let r = parseLibraryOptionClassifier x
              in  case r of 
                    Left msg -> error (show msg)
                    Right result -> result


parseLibraryOptionClassifier :: String -> Either String LibraryOption 
parseLibraryOptionClassifier [] = Left "empty option"
parseLibraryOptionClassifier str@(x:xs) = 
  case x of
    '-' -> if null xs 
             then Left "parse error"
             else let (y:ys) = xs
                  in  case y of
                        'L' -> Right (Dir ys)
                        'l' -> Right (Lib ys)
                        _ -> Right (Opt str)
    _ -> Right (Opt str) 

