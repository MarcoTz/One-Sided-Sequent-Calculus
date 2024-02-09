module Main where 

import Examples
import TwoSided.Pretty () 

import Control.Monad (forM_)

main :: IO()
main = forM_ tys (\ty -> do 
 print ty)

