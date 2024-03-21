module Files.Loader where 

import Files.Definition
import Common 
import Errors
import Syntax.Parsed.Program qualified as P
import Parser.Definition
import Parser.Program

import System.Directory 
import System.FilePath
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List (find) 
import Data.Either (isRight)
import Data.Text.IO qualified as T
import Data.Text qualified as T
import Data.Set qualified as S

listRecursive :: FilePath -> IO [Modulename]
listRecursive path = do 
  files <- listDirectory path
  paths <- forM files (\fl -> do
    isDirectory <- doesDirectoryExist fl
    if isDirectory then listRecursive fl
    else if takeExtension fl == ".os" then return [MkModule . takeBaseName $ fl] else return [])
  let paths' = concat paths
  return paths' 

findModuleFile :: Modulename -> FileLoaderM String
findModuleFile (MkModule mn) = do 
  let filePaths =  (\x -> joinPath [x, mn <> ".os"]) <$> allowedDirs
  existing <- forM filePaths (\fp -> do
    ex <- liftIO $ doesFileExist fp
    if ex then return (Right fp) else return (Left ()))
  let found = find isRight existing
  case found of 
    Nothing -> throwError (ErrModuleNotFound (MkModule mn) "findFile")
    Just (Left _) -> throwError (ErrModuleNotFound (MkModule mn) "findFile") 
    Just (Right fp) -> return fp

loadModule :: Modulename -> FileLoaderM String 
loadModule (MkModule nm) = do 
  filePath <- findModuleFile (MkModule nm)
  progText <- liftIO $ T.readFile filePath
  return (T.unpack progText)


loadProgram :: Modulename -> FileLoaderM P.Program
loadProgram mn = do
  progText <- loadModule mn
  let progParsed = runFileParser "" parseProgram progText
  progParsed' <- liftError progParsed 
  unless (P.progName progParsed' == mn) $ throwError (ErrModuleNotFound mn " wrong module name in file")
  return progParsed'

loadProgramWithImports :: Modulename -> FileLoaderM (P.Program,[P.Program])
loadProgramWithImports mn = do 
  prog <- loadProgram mn 
  let imports = P.importName <$> P.progImports prog
  imps <- loadImportsRecursive imports S.empty 
  return (prog,imps)
  where 
    loadImportsRecursive :: [Modulename] -> S.Set P.Program -> FileLoaderM [P.Program]
    loadImportsRecursive [] loaded = return $ S.toList loaded 
    loadImportsRecursive (imp1:imps) loaded | imp1 `elem` S.map P.progName loaded = loadImportsRecursive imps loaded
    loadImportsRecursive (imp1:imps) loaded = do 
      (imp1',imps') <- loadProgramWithImports imp1
      let newLoaded = S.union (S.fromList imps') (S.insert imp1' loaded)
      loadImportsRecursive imps newLoaded 
