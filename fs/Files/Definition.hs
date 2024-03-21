module Files.Definition where 

import Errors

import Control.Monad.Except
import Control.Monad.State

allowedDirs :: [FilePath] 
allowedDirs = ["Examples","CounterExamples"]

newtype FileLoaderM a = FileLoaderM { getLoaderM :: (ExceptT Error IO) a }
  deriving newtype (Functor, Applicative, Monad,MonadError Error, MonadIO)

runFileLoaderM :: FileLoaderM a -> IO (Either Error a)
runFileLoaderM m = runExceptT (getLoaderM m)

liftError :: Either Error a -> FileLoaderM a
liftError (Left err) = throwError err
liftError (Right a) = return a
