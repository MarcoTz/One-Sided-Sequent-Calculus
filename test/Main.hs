module Main where

import Driver.Definition
import Driver.Driver

import System.Directory
import System.FilePath
import Control.Monad
import Data.Either (isRight,fromLeft)
import Data.List (isInfixOf)

import Test.Tasty
import Test.Tasty.HUnit

exPath :: FilePath
exPath = "Examples"

exclude :: [String]
exclude = [""]

listRecursive :: FilePath -> IO [FilePath]
listRecursive path = do 
  files <- listDirectory path
  paths <- forM files (\fl -> do
    isDirectory <- doesDirectoryExist fl
    if isDirectory then do 
      recFiles <- listRecursive fl
      return $ (\x -> joinPath [fl, x]) <$> recFiles
    else if takeExtension fl == ".os" then return [fl] else return [])
  let paths' = concat paths
  return $ (\x -> joinPath [path,x]) <$> paths' 

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
