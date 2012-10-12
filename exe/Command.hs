{-# LANGUAGE DeriveDataTypeable #-}

module Command where

import System.Console.CmdArgs

data HROOTGenerate = Generate { config :: FilePath } 
            deriving (Show,Data,Typeable)

generate :: HROOTGenerate
generate = Generate { config = "HROOT.conf" } 

mode :: HROOTGenerate
mode = modes [generate] 

