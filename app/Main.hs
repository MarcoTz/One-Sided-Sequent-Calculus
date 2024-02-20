module Main where 

import Driver.Definition
import Driver.Driver
import Utils 
import Typed.Program

import Control.Monad
--import Data.List (isInfixOf)

colorError :: String
colorError = "\ESC[31m"
colorSuccess :: String
colorSuccess = "\ESC[32m"
colorDefault :: String
colorDefault = "\ESC[0m"


parseExample :: Bool -> FilePath -> IO()
parseExample db path = do
  let drvSt = MkDriverState db emptyProg
  res <- runDriverM drvSt (inferProgram path)
  if db then case res of 
    Left err -> do 
      putStrLn ( colorError <> "Error Parsing Example: \n\t" <> err <> colorDefault)
      putStrLn "\n=========================================================\n"
    Right _ -> do
      putStrLn (colorSuccess <> "Example " <> path <> " Parsed Successfully" <> colorDefault)
      putStrLn "\n=========================================================\n"
  else case res of 
    Left _ -> putStrLn (colorSuccess <> "Counterxexample " <> path <> " failed Successfully" <> colorDefault)
    Right _ -> putStrLn (colorError <> "Counterexample " <> path <> "did not fail" <> colorDefault)


main :: IO()
main = do 
  exPaths <- listRecursive "Examples"
  cExPaths <- listRecursive "CounterExamples"
--  let paths = filter (isInfixOf "Stream") paths
  putStrLn "========================================================="
  putStrLn "================ Testing CounterExamples ================"
  putStrLn "=========================================================" 
  forM_ cExPaths (parseExample False)
  putStrLn ""
  putStrLn "Finished Parsing Counterexamples"
  putStrLn "" 
  putStrLn "========================================================"
  putStrLn "=================== Testing Examples ==================="
  putStrLn "========================================================"
  forM_ exPaths (parseExample True)
  putStrLn ""
  putStrLn "Finished Parsing Examples"

