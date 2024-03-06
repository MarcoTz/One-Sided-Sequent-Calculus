module Main where 

import Driver.Definition
import Driver.Driver 
import Common
import Environment

import System.Environment 
import Control.Monad 


colorError :: String
colorError = "\ESC[31m"
colorSuccess :: String
colorSuccess = "\ESC[32m"
colorDefault :: String
colorDefault = "\ESC[0m"

main :: IO()
main = do
  args <- getArgs
  forM_ args runModule 

runModule :: String -> IO ()
runModule nm = do 
  putStrLn  "\n================================\n"
  putStrLn ("=====checking Module " <> nm <> "=====")
  putStrLn  "'\n================================\n"
  let st = MkDriverState True emptyEnv
  res <- runDriverM st (inferProgram (MkModule nm))
  case res of 
    Left err -> putStrLn (colorError <> "Error in module "<> nm <> ": " <> show err <> colorDefault)
    Right _  -> putStrLn (colorSuccess <> "Successfully inferred program " <> nm <> colorDefault)

