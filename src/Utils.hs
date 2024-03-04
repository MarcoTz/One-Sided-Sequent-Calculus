module Utils where 


import System.Directory 
import System.FilePath
import Control.Monad

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
