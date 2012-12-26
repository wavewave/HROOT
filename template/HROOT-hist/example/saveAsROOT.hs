module Main where

import HROOT

main = do 
  tcanvas <- newTCanvas "test" "test" 640 480
  h1 <- newTH1F "test" "test" 100 1 10
  h2 <- newTH1F "test" "test" 100 1 10 
  
  fill1 h1 5 
  fill1 h2 6
  
  add h1 h2 1.0
  
  draw h1 "" 

  tfile <- newTFile "test.root" "NEW" "" 1   
  write h1 "" 0 0 
  close tfile ""

  delete tfile
  delete h1
  delete h2

  delete tcanvas

          