module Main where 

import Examples
import Pretty () 
import Driver.Driver 

import Control.Monad (forM_)

main :: IO()
main = do
  print "Declarations "
  print debugLn
  forM_ tys (\ty -> do 
    print ty)
  print debugLn
  forM_ terms (\term -> do
    print term
    runDriverM tys (inferTerm term))
  forM_ cmds (\cmd -> do
    print cmd
    runDriverM tys (inferCommand cmd))

