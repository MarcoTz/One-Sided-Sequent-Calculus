module Kinding.Definition (
  KindM (..),
  runKindM
) where 

import Environment (Environment)
import Kinding.Errors (KindError)

import Data.Either (Either)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Except (Except, runExcept)

type KindM a = ReaderT Environment (Except KindError) a

runKindM :: forall a.Environment -> KindM a -> Either KindError a
runKindM env m = runExcept (runReaderT m env) 
