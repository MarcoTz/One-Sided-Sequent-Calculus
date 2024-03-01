module Utils where 

import Errors 

import System.Directory 
import System.FilePath
import Control.Monad
import Control.Monad.Except

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

zipWithError :: MonadError Error m => [a] -> [b] -> Error -> m [(a,b)]
zipWithError [] [] _ = return []
zipWithError [] (_:_) err = throwError err
zipWithError (_:_) [] err = throwError err
zipWithError (a1:as) (b1:bs) err = (\z -> (a1,b1) : z) <$> zipWithError as bs err
