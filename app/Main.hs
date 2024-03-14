module Main where 

import Driver.Definition
import Driver.Driver 
import Common
import Environment

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

run :: String -> IO ()
run nm = do 
  putStrLn  "\n================================\n"
  putStrLn ("=====checking Module " <> nm <> "=====")
  putStrLn  "'\n================================\n"
  let st = MkDriverState True emptyEnv 
  res <- runDriverM st (runModule (MkModule nm))
  case res of 
    Left err -> putStrLn (colorError <> "Error in module "<> nm <> ": " <> show err <> colorDefault)
    Right (Nothing,_) -> putStrLn (colorSuccess <> "Successfully inferred program " <> nm <> colorDefault)
    Right (Just c,_) -> putStrLn (colorSuccess <> "Successfully ran program " <> nm <> "\nwith result "<> show c <> colorDefault)
