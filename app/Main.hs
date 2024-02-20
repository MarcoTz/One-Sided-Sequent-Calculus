module Main where 

import Driver.Definition
import Driver.Driver
import Utils 

import Control.Monad
--import Data.List (isInfixOf)

showParsed :: Show a => Either String (a,DriverState) -> IO ()
showParsed p1 = case p1 of 
  Left err -> putStrLn ("Error Parsing Example: \n" <> err)
  Right (pr,_) -> print pr

parseExample :: FilePath -> IO()
parseExample path = do
  putStrLn (" Parsing Example " <> path)
  res <- runDriverMDb [] (inferProgram path)
  showParsed res
  putStrLn ""


main :: IO()
main = do 
  paths <- listRecursive "Examples"
--  let paths = filter (isInfixOf "Stream") paths
  print paths
  forM_ paths parseExample
  putStrLn "Finished Parsing Examples"
