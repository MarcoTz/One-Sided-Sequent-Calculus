module Main where

import Driver.Definition
import Driver.Driver
import Utils

import Control.Monad
import Data.Either (isRight,fromLeft)
import Data.List (isInfixOf)

import Test.Tasty
import Test.Tasty.HUnit

exPath :: FilePath
exPath = "Examples"

exclude :: [String]
exclude = [""]

listExamples :: IO [FilePath]
listExamples = listRecursive exPath

main :: IO()
main = do
  exPaths <- listExamples
  let exPaths' = filter (\x -> not (any (`isInfixOf` x) exclude) ) exPaths
  exParsed <- forM exPaths' (\ex -> do
    res <- runDriverMDb [] (inferProgram ex)
    let testName = "Example " <> ex
    let cs = testCase testName $ assertBool (fromLeft "" res) (isRight res)
    return cs )
  putStrLn ("Parsing examples " <> show exPaths)
  let tests = testGroup "Examples" exParsed
  defaultMain tests
