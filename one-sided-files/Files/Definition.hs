module Files.Definition (
  runFileLoaderM,
  FileLoaderM,
  liftFileError,
  allowedDirs,
  FilesError (..)
) where 

import Errors
import Loc
import Common
import Pretty.Common ()

import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)

allowedDirs :: [FilePath] 
allowedDirs = ["Examples","CounterExamples"]

data FilesError = 
  ErrModuleNotFound !Modulename
  | ErrParser !String !Loc

instance Error FilesError where 
  getMessage (ErrModuleNotFound mn) = "Module " <> show mn <> " not found in " <> intercalate ", " (show <$> allowedDirs)
  getMessage (ErrParser str _) = "Error parsing: " <> show str

  getLoc _ = defaultLoc
  toError= ErrParser

newtype FileLoaderM a = FileLoaderM { getLoaderM :: (ExceptT FilesError IO) a }
  deriving newtype (Functor, Applicative, Monad,MonadError FilesError, MonadIO)

runFileLoaderM :: FileLoaderM a -> IO (Either FilesError a)
runFileLoaderM m = runExceptT (getLoaderM m)

liftFileError :: Error e => Either e a -> FileLoaderM a
liftFileError (Left err) = throwError (toError (getMessage err) (getLoc err))
liftFileError (Right a) = return a
