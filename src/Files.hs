module Files where 

import Common 
import Errors

import System.Directory 
import System.FilePath
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text.IO qualified as T
import Data.Text qualified as T

listRecursive :: FilePath -> IO [Modulename]
listRecursive path = do 
  files <- listDirectory path
  paths <- forM files (\fl -> do
    isDirectory <- doesDirectoryExist fl
    if isDirectory then listRecursive fl
    else if takeExtension fl == ".os" then return [MkModule . takeBaseName $ fl] else return [])
  let paths' = concat paths
  return paths' 

loadModule :: MonadIO m => MonadError Error m =>  Modulename -> m T.Text
loadModule (MkModule nm) = do 
  let filePath =  joinPath ["Examples", nm <> ".os"]
  exists <- liftIO $ doesFileExist filePath
  unless exists $ throwError (ErrModuleNotFound (MkModule nm) "loadModule")
  liftIO $ T.readFile filePath
