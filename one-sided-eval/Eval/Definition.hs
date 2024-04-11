module Eval.Definition (
  EvalM,
  runEvalM,
  EvalError (..),
  EvalTrace (..),
  emptyTrace
) where

import Environment 
import Errors 
import Loc
import Common
import Syntax.Kinded.Terms

import Control.Monad.Except 
import Control.Monad.Reader

data EvalError where 
  ErrXtorArity :: Loc -> Xtorname -> EvalError
  ErrMissingPt :: Loc -> Xtorname -> EvalError
  ErrOther     :: Loc -> String   -> EvalError

instance Error EvalError where 
  getMessage (ErrXtorArity _ xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrMissingPt _ xtn) = "No pattern for xtor " <> show xtn 
  getMessage (ErrOther _ str) = str

  getLocation (ErrXtorArity loc _) = loc 
  getLocation (ErrMissingPt loc _) = loc 
  getLocation (ErrOther loc _) = loc 

  toError = ErrOther 

data EvalTrace = MkTrace !Command ![Command] 
  deriving (Eq)

emptyTrace :: EvalTrace 
emptyTrace = MkTrace (Done defaultLoc) []

newtype EvalM a = MkEvalM { getEvalM :: ReaderT Environment (Except EvalError) a }
  deriving newtype (Functor, Applicative, Monad, MonadError EvalError, MonadReader Environment)

runEvalM :: Environment -> EvalM a -> Either EvalError a
runEvalM env m = runExcept (runReaderT (getEvalM m) env)
