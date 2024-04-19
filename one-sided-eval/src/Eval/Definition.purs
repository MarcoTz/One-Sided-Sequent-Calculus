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

import Loc (Loc)
import Common (Xtorname)
import Errors (class Error)
import Environment (Environment)
import Syntax.Kinded.Terms (Command)

import Prelude ((<>), class Show, show, class Eq, (<$>), (==), (||))
import Data.List (List(..),intercalate, elem)
import Data.Either (Either)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Except (Except, runExcept)

data EvalError = 
  ErrXtorArity    Loc Xtorname 
  | ErrMissingPt  Loc Xtorname
  | ErrLoop       Loc Command
  | ErrTwoCase    Loc Command
  | ErrTwoXtor    Loc Command
  | ErrOther      Loc String

instance Error EvalError where
  getMessage (ErrXtorArity _ xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrMissingPt _ xtn) = "No pattern for xtor " <> show xtn 
  getMessage (ErrLoop _ c) = "Cannot evaluate " <> show c <> ", evaluation results in loop"
  getMessage (ErrTwoCase _ c) = "Cannot evaluate " <> show c <> ", cut between cases"
  getMessage (ErrTwoXtor _ c) = "Cannot evaluate " <> show c <> ", cut between xtors"
  getMessage (ErrOther _ str) = str

  getLocation (ErrXtorArity loc _) = loc 
  getLocation (ErrMissingPt loc _) = loc 
  getLocation (ErrLoop loc _) = loc
  getLocation (ErrTwoCase loc _) = loc
  getLocation (ErrTwoXtor loc _) = loc
  getLocation (ErrOther loc _) = loc 

  toError = ErrOther 

data EvalTrace = MkTrace Command (List Command)
derive instance eqTrace :: Eq EvalTrace
instance Show EvalTrace where 
  show (MkTrace c Nil) = "Result " <> show c
  show (MkTrace c tr) = "Result " <> show c <> "\nTrace:\n" <> intercalate "\n" (show <$> tr)
appendTrace :: EvalTrace -> Command -> EvalTrace 
appendTrace (MkTrace c tr) c' = MkTrace c' (tr <> (Cons c Nil))

prependTrace :: EvalTrace -> Command -> EvalTrace 
prependTrace (MkTrace c tr) c' = MkTrace c (Cons c' tr)

emptyTrace :: Command -> EvalTrace 
emptyTrace c = MkTrace c Nil

inTrace :: EvalTrace -> Command -> Boolean
inTrace (MkTrace c tr) c' = c==c' || c `elem` tr

type EvalM a = ReaderT Environment (Except EvalError) a

runEvalM :: forall a.Environment -> EvalM a -> Either EvalError a
runEvalM env m = runExcept (runReaderT m env)
