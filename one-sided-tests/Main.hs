module Main where 

import Driver.Definition
import Driver.Driver
import Files.Loader
import Files.Definition
import Environment
import Common
import Errors
import Syntax.Parsed.Program

import Control.Monad
import Data.List (sort)

colorError :: String
colorError = "\ESC[31m"
colorSuccess :: String
colorSuccess = "\ESC[32m"
colorDefault :: String
colorDefault = "\ESC[0m"

printSucc :: Modulename -> Bool -> IO () 
printSucc mn False = putStrLn (colorSuccess <> "Example " <> show mn <> " inferred successfully " <> colorDefault)
printSucc mn True = putStrLn (colorError <> "CounterExample " <> show mn <> " did not fail " <> colorDefault)

printErr :: Error e => String -> Modulename -> Bool -> e -> IO () 
printErr src mn False err  = putStrLn (colorError <> "Error Checking Example " <> show mn <> "\nError: " <> showInSrc err src <> colorDefault)
printErr _ mn True _ = putStrLn (colorSuccess <> "Counterexmaples " <> show mn <> " failed successfully" <> colorDefault)

printRes :: Error e => String -> Modulename -> Bool -> Either e a -> IO () 
printRes src mn shouldFail (Left err) = printErr src mn shouldFail err
printRes _ mn shouldFail (Right _) = printSucc mn shouldFail 

parseExample :: Bool -> Modulename -> IO()
parseExample shouldFail mn = do
  res <- runFileLoaderM (loadProgramWithImports mn)
  case res of 
    Left err -> putStrLn (colorError <> " Error parsing " <> show mn <> " with error " <> showWithLoc err <> colorDefault)
    Right (prog,imps) -> do
      let drvSt = MkDriverState False emptyEnv
      res' <- runDriverM drvSt (inferAndRun prog imps False)
      printRes (progSrc prog) mn shouldFail res'


main :: IO()
main = do 
  exPaths <- listRecursive "Examples"
  cExPaths <- sort <$> listRecursive "CounterExamples"
  putStrLn "========================================================="
  putStrLn "================ Testing CounterExamples ================"
  putStrLn "=========================================================" 
  forM_ cExPaths (parseExample True)
  putStrLn ""
  putStrLn "Finished testing Counterexamples"
  putStrLn "" 
  putStrLn "========================================================"
  putStrLn "=================== Testing Examples ==================="
  putStrLn "========================================================"
  forM_ exPaths (parseExample False)
  putStrLn ""
  putStrLn "Finished Running Examples"
