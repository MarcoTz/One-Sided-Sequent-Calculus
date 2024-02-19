module Main where 

import Examples
import Pretty () 
import Driver.Driver 

import Control.Monad (forM_)

main :: IO()
main = do
  forM_ tys (\ty -> do 
    print ty)
  forM_ terms (\term -> do
    print term
    print (runInfT tys term))
  forM_ cmds (\cmd -> do
    print cmd
    print (runInfC tys cmd))

