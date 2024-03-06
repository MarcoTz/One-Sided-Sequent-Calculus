module Main where 

import Driver.Definition
import Driver.Driver
import Files 
import Environment
import Common
import Pretty.Errors ()

import Control.Monad
--import Data.List (isInfixOf)

colorError :: String
colorError = "\ESC[31m"
colorSuccess :: String
colorSuccess = "\ESC[32m"
colorDefault :: String
colorDefault = "\ESC[0m"


parseExample :: Bool -> Modulename -> IO()
parseExample shouldFail mn = do
  let drvSt = MkDriverState False emptyEnv 
  res <- runDriverM drvSt (inferProgram mn)
  if shouldFail then case res of 
    Left err -> do 
      putStrLn ( colorError <> "Error Checking Example: \n\t" <> show err <> colorDefault)
      putStrLn "\n=========================================================\n"
    Right _ -> do
      putStrLn (colorSuccess <> "Example " <> show mn <> " Checked Successfully" <> colorDefault)
      putStrLn "\n=========================================================\n"
  else case res of 
    Left _ -> putStrLn (colorSuccess <> "Counterxexample " <> show mn <> " failed Successfully" <> colorDefault)
    Right _ -> putStrLn (colorError <> "Counterexample " <> show mn <> " did not fail" <> colorDefault)


main :: IO()
main = do 
  exPaths <- listRecursive "Examples"
  cExPaths <- listRecursive "CounterExamples"
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
