module TypeInference.SolveConstraints.Definition where 

import TypeInference.Constraints
import Syntax.Typed.Types
import Common 
import Errors
import Pretty.Common ()
import Pretty.Types ()

import Data.Map qualified as M
import Control.Monad.Except
import Control.Monad.State

--
-- Solver Monad 
-- 
data SolverState = MkSolverState 
  { 
  slvTyVars :: !(M.Map TypeVar Ty), 
  slvKndVars :: !(M.Map KindVar Pol),
  remConstrs :: ![Constraint]
}

initialSolverState :: [Constraint] -> SolverState
initialSolverState = MkSolverState M.empty M.empty 

newtype SolverM a = MkSolveM { getSolveM :: StateT SolverState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState SolverState, MonadError Error)

runSolveM :: [Constraint] -> SolverM a -> Either Error (a,M.Map TypeVar Ty, M.Map KindVar Pol)
runSolveM constrs m = case runExcept (runStateT (getSolveM m) (initialSolverState constrs) ) of 
  Left err -> Left err 
  Right (x,st) -> Right (x,slvTyVars st, slvKndVars st)
