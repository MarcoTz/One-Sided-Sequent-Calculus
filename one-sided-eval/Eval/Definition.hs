module Eval.Definition (
  EvalM,
  runEvalM,
  EvalError (..),
  EvalTrace (..),
  appendTrace,
  prependTrace,
  inTrace,
  emptyTrace
) where

import Environment 
import Errors 
import Loc
import Common
import Syntax.Kinded.Terms
import Pretty.Kinded()

import Control.Monad.Except 
import Control.Monad.Reader

data EvalError where 
  ErrXtorArity :: Loc -> Xtorname -> EvalError
  ErrMissingPt :: Loc -> Xtorname -> EvalError
  ErrLoop      :: Loc -> Command  -> EvalError
  ErrOther     :: Loc -> String   -> EvalError

instance Error EvalError where 
  getMessage (ErrXtorArity _ xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrMissingPt _ xtn) = "No pattern for xtor " <> show xtn 
  getMessage (ErrLoop _ c) = "Cannot evaluate " <> show c <> ", evaluation results in loop"
  getMessage (ErrOther _ str) = str

  getLocation (ErrXtorArity loc _) = loc 
  getLocation (ErrMissingPt loc _) = loc 
  getLocation (ErrLoop loc _) = loc
  getLocation (ErrOther loc _) = loc 

  toError = ErrOther 

data EvalTrace = MkTrace !Command ![Command] 
  deriving (Eq)

appendTrace :: EvalTrace -> Command -> EvalTrace 
appendTrace (MkTrace c tr) c' = MkTrace c' (tr ++ [c])

prependTrace :: EvalTrace -> Command -> EvalTrace 
prependTrace (MkTrace c tr) c' = MkTrace c (c':tr)

emptyTrace :: Command -> EvalTrace 
emptyTrace c = MkTrace c []

inTrace :: EvalTrace -> Command -> Bool
inTrace (MkTrace c tr) c' = c==c' || c `elem` tr

newtype EvalM a = MkEvalM { getEvalM :: ReaderT Environment (Except EvalError) a }
  deriving newtype (Functor, Applicative, Monad, MonadError EvalError, MonadReader Environment)

runEvalM :: Environment -> EvalM a -> Either EvalError a
runEvalM env m = runExcept (runReaderT (getEvalM m) env)
