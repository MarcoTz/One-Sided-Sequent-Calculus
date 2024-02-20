module Main where 

import Driver.Definition
import Driver.Driver
import Utils 

import Control.Monad
--import Data.List (isInfixOf)

colorError :: String
colorError = "\ESC[31m"
colorDefault :: String
colorDefault = "\ESC[0m"

showParsed :: FilePath -> Either String a -> IO ()
showParsed path p1 = case p1 of 
  Left err -> putStrLn ( colorError <> "Error Parsing Example: \n\t" <> err <> colorDefault)
  Right _ -> putStrLn ("Example " <> path <> " Parsed Successfully")

parseExample :: FilePath -> IO()
parseExample path = do
  putStrLn ("Parsing Example " <> path)
  res <- runDriverMDb [] (inferProgram path)
  showParsed path res
  putStrLn ""


main :: IO()
main = do 
  exPaths <- listRecursive "Examples"
  cExPaths <- listRecursive "CounterExamples"
--  let paths = filter (isInfixOf "Stream") paths
  putStrLn "================== Testing Examples =================="
  forM_ exPaths parseExample
  putStrLn "Finished Parsing Examples"
  putStrLn ""
  putStrLn "=============== Testing CounterExamples ===============" 
  forM_ cExPaths parseExample
  putStrLn "Finished Parsing Counterexamples"
