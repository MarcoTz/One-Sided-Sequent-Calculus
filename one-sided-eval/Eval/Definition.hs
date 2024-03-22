module Eval.Definition (
  EvalM,
  runEvalM
) where

import Environment 
import Errors 

import Control.Monad.Except 
import Control.Monad.Reader

newtype EvalM a = MkEvalM { getEvalM :: ReaderT Environment (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Error, MonadReader Environment)

runEvalM :: Environment -> EvalM a -> Either Error a
runEvalM env m = runExcept (runReaderT (getEvalM m) env)
