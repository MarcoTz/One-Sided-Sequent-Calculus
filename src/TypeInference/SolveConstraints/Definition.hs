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
  remConstrs :: !ConstraintSet 
}

initialSolverState :: ConstraintSet -> SolverState
initialSolverState = MkSolverState M.empty M.empty 

newtype SolverM a = MkSolveM { getSolveM :: StateT SolverState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState SolverState, MonadError Error)

runSolveM :: ConstraintSet -> SolverM a -> Either Error (a,M.Map TypeVar Ty, M.Map KindVar Pol)
runSolveM constrs m = case runExcept (runStateT (getSolveM m) (initialSolverState constrs) ) of 
  Left err -> Left err 
  Right (x,st) -> Right (x,slvTyVars st, slvKndVars st)

insertConstraint :: Constraint -> SolverM ()
insertConstraint constr = do
  (MkConstraintSet constrs) <- gets remConstrs
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet (constr:constrs)))

addConstraintsArgs :: TypeName -> [Ty] -> [Ty] -> SolverM () 
addConstraintsArgs _ [] [] = return () 
addConstraintsArgs tyn [] _ = throwError (ErrArityTy tyn)
addConstraintsArgs tyn _ [] = throwError (ErrArityTy tyn)
addConstraintsArgs tyn (ty1:tys1) (ty2:tys2) = do
  insertConstraint (MkTyEq ty1 ty2)
  addConstraintsArgs tyn tys1 tys2

