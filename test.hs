import HROOT
import HROOT.RooFit
import HROOT.RooFit.RooStats
-- import HROOT.RooFit.RooRandom.Implementation


main = do 
  rooRandomrandomGenerator >>= \rg -> setSeed rg 3001
  
  wspace <- newRooWorkspace 
  factory wspace "Gaussian::normal(x[-10,10],mu[-1,1],sigma[1])"
  defineSet wspace "poi" "mu" 
  defineSet wspace "obs" "x" 

  modelConfig <- newModelConfig "Example G(x|mu,1)" "Example G(x|mu,1)"
  setWorkspace modelConfig wspace
  setPdf modelConfig =<< pdf wspace "normal"
  setParametersOfInterest modelConfig =<< HROOT.RooFit.set wspace "poi"
  setObservables modelConfig =<< HROOT.RooFit.set wspace "obs"

  dat <- pdf wspace "normal" >>= \p -> HROOT.RooFit.set wspace "obs" >>= \a -> generate p a 100 
  printObj dat "" 

  x <- var wspace "x"
  mu <- var wspace "mu" 

  let confidenceLevel = 0.95 :: Double 
  plc <- newProfileLikelihoodCalculator (upcastRooAbsData dat) modelConfig 
  plInt <- getInterval plc 

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
