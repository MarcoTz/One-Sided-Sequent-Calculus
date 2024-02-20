module Main where 

import Driver.Definition
import Driver.Driver
import Utils 

import Control.Monad
import Data.List (isInfixOf)

showParsed :: Show a => [Either String (a,DriverState)] -> IO ()
showParsed [] = putStrLn "Finished Parsing Examples"
showParsed (p1:ps) = case p1 of 
  Left err -> putStrLn ("Error Parsing Example: \n" <> err)
  Right (pr,_) -> do 
    print pr
    showParsed ps

main :: IO()
main = do 
  paths <- listRecursive "Examples"
  let paths' = filter (isInfixOf "Stream") paths
  print paths'
  parsed <- forM paths' (runDriverMDb [] . inferProgram)
  showParsed parsed

