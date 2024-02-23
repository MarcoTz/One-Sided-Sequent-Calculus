module TypeInference.SolveConstraints.Solver where 

import TypeInference.SolveConstraints.Definition
import TypeInference.Constraints
import Syntax.Typed.Types
import Common
import Errors 

import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M

solve :: SolverM () 
solve = do
  (MkConstraintSet ctrs) <- gets remConstrs
  case ctrs of 
    [] -> return ()
    (ctr1:ctrs') -> do 
      modify (\s -> MkSolverState (slvTyVars s) (MkConstraintSet ctrs'))
      case ctr1 of 
        MkTyEq ty1 ty2 -> do 
          unifyTypeConstraint ty1 ty2 
          solve

addTyVar :: Variable -> Ty -> SolverM ()
addTyVar v ty = do 
  vars <- gets slvTyVars
  modify $ MkSolverState (M.insert v ty vars) . remConstrs

addTyEq :: Ty -> Ty -> SolverM () 
addTyEq ty1 ty2 = do 
  (MkConstraintSet constrs) <- gets remConstrs 
  modify (\s -> MkSolverState (slvTyVars s) (MkConstraintSet (MkTyEq ty1 ty2 : constrs)))

  
unifyTypeConstraint :: Ty -> Ty -> SolverM ()
unifyTypeConstraint (TyVar v) ty = do
  vars <- gets slvTyVars
  case M.lookup v vars of 
    Nothing -> addTyVar v ty 
    Just ty' -> addTyEq ty' ty
unifyTypeConstraint ty1 ty2@TyVar{} = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1) ty2@(TyDecl n2 args2) 
 | n1 /= n2 = throwError (ErrTyNeq ty1 ty2)
 | otherwise = do 
     addConstraintsArgs n1 args1 args2

unifyTypeConstraint (TyShift ty1) (TyShift ty2) = do 
  unifyTypeConstraint ty1 ty2
unifyTypeConstraint (TyCo ty1) (TyCo ty2) = do 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTyNeq ty1 ty2)
