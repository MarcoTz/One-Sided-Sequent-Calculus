module Eval.Definition (
  EvalM,
  runEvalM,
  EvalError (..)
) where

import Environment 
import Errors 
import Loc
import Common

import Control.Monad.Except 
import Control.Monad.Reader

data EvalError = 
  ErrXtorArity !XtorName
  | ErrMissingPt !XtorName

instance Error EvalError where 
  getMessage (ErrXtorArity xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrMissingPt xtn) = "No pattern for xtor " <> show xtn 
  getLoc _  = defaultLoc
  toError _ = error "not implemented" 

newtype EvalM a = MkEvalM { getEvalM :: ReaderT Environment (Except EvalError) a }
  deriving newtype (Functor, Applicative, Monad, MonadError EvalError, MonadReader Environment)

runEvalM :: Environment -> EvalM a -> Either EvalError a
runEvalM env m = runExcept (runReaderT (getEvalM m) env)
