module SolveConstraints.Definition (
  SolverM,
  runSolveM,
  ConstrTy (..),
  addSlvKndVar,
  getSlvKndVars,
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
  slvTyVars :: !(M.Map PolVar Ty), 
  slvKndVars :: !(M.Map KindVar Pol),
  remConstrs :: !ConstraintSet 
}

data ConstrTy = Eq | Neq

initialSolverState :: ConstraintSet -> SolverState
initialSolverState = MkSolverState M.empty M.empty

newtype SolverM a = MkSolveM { getSolveM :: StateT SolverState (Except SolverError) a }
  deriving newtype (Functor, Applicative, Monad, MonadState SolverState, MonadError SolverError)

runSolveM :: ConstraintSet -> SolverM a -> Either SolverError (a,M.Map PolVar Ty,M.Map KindVar Pol)
runSolveM constrs m = case runExcept (runStateT (getSolveM m) (initialSolverState constrs) ) of 
  Left err -> Left err 
  Right (x,st) -> Right (x,slvTyVars st,slvKndVars st)

getNextConstr :: SolverM (Maybe Constraint)
getNextConstr = do 
  (MkConstraintSet constrs) <- gets remConstrs 
  case constrs of 
    [] -> return Nothing 
    (c1:ctrs) -> do 
      modify (\s  -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet ctrs))
      return (Just c1)

getSlvTyVars :: SolverM (M.Map PolVar Ty)
getSlvTyVars = gets slvTyVars 

addSlvTyVar :: PolVar -> Ty -> SolverM ()
addSlvTyVar v ty = do 
  vars <- gets slvTyVars
  modify (\s -> MkSolverState (M.insert v ty vars) (slvKndVars s) (remConstrs s))

getSlvKndVars :: SolverM (M.Map KindVar Pol)
getSlvKndVars = gets slvKndVars

addSlvKndVar :: KindVar -> Pol -> SolverM () 
addSlvKndVar kv pol = do
  kndVars <- gets slvKndVars 
  modify (\s -> MkSolverState (slvTyVars s) (M.insert kv pol kndVars) (remConstrs s))

addConstraint :: Constraint -> SolverM ()
addConstraint constr = do
  (MkConstraintSet constrs) <- gets remConstrs
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet (constr:constrs)))

addConstraintsArgs :: TypeName -> [Ty] -> [Ty] -> SolverM () 
addConstraintsArgs _ [] [] = return () 
addConstraintsArgs tyn [] _ = throwError (ErrTyArity tyn)
addConstraintsArgs tyn _ [] = throwError (ErrTyArity tyn)
addConstraintsArgs tyn (ty1:tys1) (ty2:tys2) = do
  addConstraint (MkTyEq ty1 ty2)
  addConstraintsArgs tyn tys1 tys2

