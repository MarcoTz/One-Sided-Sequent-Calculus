module Kinding.Definition (
  KindM (..),
  runKindM
) where 

import Kinding.Errors
import Environment 

import Control.Monad.Reader 
import Control.Monad.Except


newtype KindM a = KindM { getKindM :: ReaderT Environment (Except KindError) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Environment, MonadError KindError)

runKindM :: Environment -> KindM a -> Either KindError a
runKindM env m = runExcept (runReaderT (getKindM m) env) 
