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

data FilesError where
  ErrModuleNotFound :: Modulename -> FilesError
  ErrOther :: Loc -> String -> FilesError

instance Error FilesError where 
  getMessage (ErrModuleNotFound mn) = "Module " <> show mn <> " not found in " <> intercalate ", " (show <$> allowedDirs)
  getMessage (ErrOther _ str) = "Error parsing: " <> show str

  getLocation (ErrModuleNotFound _)  = defaultLoc
  getLocation (ErrOther loc _) = loc

  toError= ErrOther

newtype FileLoaderM a = FileLoaderM { getLoaderM :: (ExceptT FilesError IO) a }
  deriving newtype (Functor, Applicative, Monad,MonadError FilesError, MonadIO)

runFileLoaderM :: FileLoaderM a -> IO (Either FilesError a)
runFileLoaderM m = runExceptT (getLoaderM m)

liftFileError :: Error e => Either e a -> FileLoaderM a
liftFileError (Left err) = throwError (convertError err) 
liftFileError (Right a) = return a
