module SolveConstraints.Definition (
  SolverM,
  runSolveM,
  ConstrTy (..),
  addSlvTyVar,
  getSlvTyVars,
  getNextConstr,
  addConstraintsArgs,
  addConstraint,
) where 

import SolveConstraints.Errors
import Constraints
import Syntax.Typed.Types
import Common 
import Pretty.Common ()
import Pretty.Typed ()

import Data.Map qualified as M
import Control.Monad.Except
import Control.Monad.State

--
-- Solver Monad 
-- 
data SolverState = MkSolverState 
  { 
  slvTyVars :: !(M.Map Typevar Ty), 
  remConstrs :: !ConstraintSet 
}

data ConstrTy = Eq | Neq

initialSolverState :: ConstraintSet -> SolverState
initialSolverState = MkSolverState M.empty 

newtype SolverM a = MkSolveM { getSolveM :: StateT SolverState (Except SolverError) a }
  deriving newtype (Functor, Applicative, Monad, MonadState SolverState, MonadError SolverError)

runSolveM :: ConstraintSet -> SolverM a -> Either SolverError (a,M.Map Typevar Ty)
runSolveM constrs m = case runExcept (runStateT (getSolveM m) (initialSolverState constrs) ) of 
  Left err -> Left err 
  Right (x,st) -> Right (x,slvTyVars st)

getNextConstr :: SolverM (Maybe Constraint)
getNextConstr = do 
  (MkConstraintSet constrs) <- gets remConstrs 
  case constrs of 
    [] -> return Nothing 
    (c1:ctrs) -> do 
      modify (\s  -> MkSolverState (slvTyVars s) (MkConstraintSet ctrs))
      return (Just c1)

getSlvTyVars :: SolverM (M.Map Typevar Ty)
getSlvTyVars = gets slvTyVars 

addSlvTyVar :: Typevar -> Ty -> SolverM ()
addSlvTyVar v ty = do 
  vars <- gets slvTyVars
  modify $ MkSolverState (M.insert v ty vars) . remConstrs

addConstraint :: Constraint -> SolverM ()
addConstraint constr = do
  (MkConstraintSet constrs) <- gets remConstrs
  modify (\s -> MkSolverState (slvTyVars s) (MkConstraintSet (constr:constrs)))

addConstraintsArgs :: Typename -> [Ty] -> [Ty] -> SolverM () 
addConstraintsArgs _ [] [] = return () 
addConstraintsArgs tyn [] _ = throwError (ErrTyArity tyn)
addConstraintsArgs tyn _ [] = throwError (ErrTyArity tyn)
addConstraintsArgs tyn (ty1:tys1) (ty2:tys2) = do
  addConstraint (MkTyEq ty1 ty2)
  addConstraintsArgs tyn tys1 tys2

