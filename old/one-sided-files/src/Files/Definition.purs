module Files.Definition (
  runFileLoaderM,
  FileLoaderM,
  liftFileError,
  allowedDirs,
  FilesError (..)
) where 

import Common (Modulename)
import Data.List (List(..))

allowedDirs :: List FilePath
allowedDirs = Cons "Examples" (Cons "CounterExamples" Nil)

data FilesError = 
  ErrModuleNotFound Modulename 
  | ErrOther        Loc String 

instance Error FilesError where 
  getMessage (ErrModuleNotFound mn) = "Module " <> show mn <> " not found in " <> intercalate ", " (show <$> allowedDirs)
  getMessage (ErrOther _ str) = "Error parsing: " <> show str

  getLocation (ErrModuleNotFound _)  = defaultLoc
  getLocation (ErrOther loc _) = loc

  toError= ErrOther

newtype FileLoaderM a = (ExceptT FilesError IO) 

runFileLoaderM :: FileLoaderM a -> IO (Either FilesError a)
runFileLoaderM m = runExceptT (getLoaderM m)

liftFileError :: Error e => Either e a -> FileLoaderM a
liftFileError (Left err) = throwError (convertError err) 
liftFileError (Right a) = return a
