module Main where

import Driver.Definition
import Driver.Driver
import Utils

import Control.Monad
import Data.Either (isRight,isLeft,fromLeft)

import Test.Tasty
import Test.Tasty.HUnit

exPath :: FilePath
exPath = "Examples"
cExPath :: FilePath
cExPath = "CounterExamples"

main :: IO()
main = do
  exPaths <- listRecursive exPath 
  cExPaths <- listRecursive cExPath
  
  exParsed <- forM exPaths (\ex -> do
    res <- runDriverMDb [] (inferProgram ex)
    let testName = "Example " <> ex
    let cs = testCase testName $ assertBool (fromLeft "" res) (isRight res)
    return cs )

  cExParsed <- forM cExPaths (\cex -> do 
    res <- runDriverMDb [] (inferProgram cex)
    let testName = "Counterexample " <> cex
    let cs = testCase testName $ assertBool ("Counterexample " <> cex <> " could be successfully parsed ") (isLeft res)
    return cs)
  putStrLn ("Parsing examples " <> show exPaths)
  let extests = testGroup "Examples" exParsed
  let cextests = testGroup "CounterExamples" cExParsed
  let tests = testGroup "All" [extests,cextests] 
  defaultMain tests 
