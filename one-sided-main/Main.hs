module Main where 

import Driver.Definition
import Driver.Driver 
import Files.Definition
import Files.Loader
import Common
import Errors
import Environment
import Syntax.Parsed.Program
import Syntax.Kinded.Terms
import Eval.Definition
import Pretty.Eval () 

import System.Environment 
import Control.Monad 


colorError   :: String
colorError   = "\ESC[31m"
colorSuccess :: String
colorSuccess = "\ESC[32m"
colorDefault :: String
colorDefault = "\ESC[0m"

main :: IO()
main = do
  args <- getArgs
  forM_ args run

showRes :: Either Command EvalTrace -> String
showRes (Left c) = " result " <> show c 
showRes (Right tr) = " result " <> show tr 

run :: String -> IO ()
run nm = do 
  putStrLn  "\n================================\n"
  putStrLn ("=====checking Module " <> nm <> "=====")
  putStrLn  "'\n================================\n"
  loaded <- runFileLoaderM (loadProgramWithImports (Modulename nm))
  case loaded of 
    Left err -> putStrLn (colorError <> " Error parsing module " <> nm <> ": " <> showWithLoc err) 
    Right (prog, imps) -> do 
      let st = MkDriverState True emptyEnv 
      res <- runDriverM st (inferAndRun prog imps True)
      case res of 
        Left err -> putStrLn (colorError <> "Error in module "<> nm <> ": " <> showInSrc err (progSrc prog) <> colorDefault)
        Right (res',_) -> putStrLn (colorSuccess <> "Successfully ran program " <> nm <> showRes res' <> colorDefault)

