{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (liftM)
import HROOT
import HROOT.RooFit
import HROOT.RooFit.RooStats
-- import HROOT.RooFit.RooRandom.Implementation


main = do 
  rooRandomrandomGenerator >>= \rg -> setSeed rg 3001
  
  wspace <- newRooWorkspace 
  rooWorkspacefactory wspace "Gaussian::normal(x[-10,10],muu[-1,1],sigma[1])"
  rooWorkspacedefineSet wspace "poi" "muu" 
  rooWorkspacedefineSet wspace "obs" "x" 

  modelConfig <- newModelConfig "Example G(x|muu,1)" "Example G(x|muu,1)"
  setWorkspace modelConfig wspace
  setPdf modelConfig =<< rooWorkspacepdf wspace "normal"
  setParametersOfInterest modelConfig =<< rooWorkspaceset wspace "poi"
  setObservables modelConfig =<< rooWorkspaceset wspace "obs"

  dat <- rooWorkspacepdf wspace "normal" >>= \p -> rooWorkspaceset wspace "obs" >>= \a -> generate p a 100 
  printObj dat "" 

  x <- rooWorkspacevar wspace "x"
  muu <- rooWorkspacevar wspace "muu" 

  let confidenceLevel = 0.95 :: Double 
  plc <- newProfileLikelihoodCalculator (upcastRooAbsData dat) modelConfig 
  (plInt :: LikelihoodInterval) <- liftM downcastConfInterval (getInterval_IntervalCalculator plc)

  printObj plInt "" 

  canvas <- newTCanvas "canvas" "canvas" 600 300 
  
  divide_tvirtualpad canvas 2 1 0.01 0.01 0 
  
  cd canvas 1 

  frm <- frame x 
  plotOn dat frm
  statOn dat frm 
  draw frm "" 
  
  cd canvas 2 
  
  plt <- newLikelihoodIntervalPlot plInt
  draw plt "" 

  saveAs canvas "mytest.pdf" "" 

  putStrLn "test2"
