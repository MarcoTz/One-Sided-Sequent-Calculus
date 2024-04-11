module Kinding.Definition where 

import Kinding.Errors
import Environment 

import Control.Monad.Reader 
import Control.Monad.Except


newtype KindM a = KindM { getKindM :: ReaderT Environment (Except KindError) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Environment, MonadError KindError)

runCheckM :: Environment -> KindM a -> Either KindError a
runCheckM env m = runExcept (runReaderT (getKindM m) env) 
