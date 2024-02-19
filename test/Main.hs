module Main where


import System.Directory
import System.FilePath
import Control.Monad

exPath :: FilePath
exPath = "Examples"


listRecursive :: FilePath -> IO [FilePath]
listRecursive path = do 
  files <- listDirectory exPath
  paths <- forM files (\fl -> do
    isDirectory <- doesDirectoryExist path
    if isDirectory then listRecursive fl
    else if takeExtension fl == ".os" then return [fl] else return [])
  return $ concat paths

listExamples :: IO [FilePath]
listExamples = listRecursive exPath

main :: IO()
main = do
  exPaths <- listExamples
  print ""

