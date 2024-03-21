module Main where 

import Driver.Definition
import Driver.Driver
import Files.Loader
import Files.Definition
import Environment
import Common
import Errors
import Pretty.Errors ()

import Control.Monad

colorError :: String
colorError = "\ESC[31m"
colorSuccess :: String
colorSuccess = "\ESC[32m"
colorDefault :: String
colorDefault = "\ESC[0m"

printSucc :: Modulename -> Bool -> IO () 
printSucc mn False = putStrLn (colorSuccess <> "Example " <> show mn <> " inferred successfully " <> colorDefault)
printSucc mn True = putStrLn (colorError <> "CounterExample " <> show mn <> " did not fail " <> colorDefault)

printErr :: Modulename -> Bool -> Error -> IO () 
printErr mn False err  = putStrLn (colorError <> "Error Checking Example " <> show mn <> "\nError: " <> show err <> colorDefault)
printErr mn True _ = putStrLn (colorSuccess <> "Counterexmaples " <> show mn <> " failed successfully" <> colorDefault)

printRes :: Modulename -> Bool -> Either Error a -> IO () 
printRes mn shouldFail (Left err) = printErr mn shouldFail err
printRes mn shouldFail (Right _) = printSucc mn shouldFail 

parseExample :: Bool -> Modulename -> IO()
parseExample shouldFail mn = do
  res <- runFileLoaderM (loadProgramWithImports mn)
  case res of 
    Left err -> putStrLn (colorError <> " Error parsing " <> show mn <> " with error " <> show err <> colorDefault)
    Right (prog,imps) -> do
      let drvSt = MkDriverState False emptyEnv
      res' <- runDriverM drvSt (inferAndRun prog imps)
      printRes mn shouldFail res'


main :: IO()
main = do 
  exPaths <- listRecursive "Examples"
  cExPaths <- listRecursive "CounterExamples"
  putStrLn "========================================================="
  putStrLn "================ Testing CounterExamples ================"
  putStrLn "=========================================================" 
  forM_ cExPaths (parseExample True)
  putStrLn ""
  putStrLn "Finished Parsing Counterexamples"
  putStrLn "" 
  putStrLn "========================================================"
  putStrLn "=================== Testing Examples ==================="
  putStrLn "========================================================"
  forM_ exPaths (parseExample False)
  putStrLn ""
  putStrLn "Finished Running Examples"
