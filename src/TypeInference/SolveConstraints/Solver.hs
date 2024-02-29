module TypeInference.SolveConstraints.Solver where 

import TypeInference.SolveConstraints.Definition
import TypeInference.Constraints
import Syntax.Typed.Types
import Common
import Errors 

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M

solve :: SolverM () 
solve = do
  (MkConstraintSet ctrs) <- gets remConstrs
  case ctrs of 
    [] -> return ()
    (ctr1:ctrs') -> do 
      modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet ctrs'))
      case ctr1 of 
        MkTyEq ty1 ty2 -> unifyTypeConstraint ty1 ty2  >> solve
        MkKindEq knd1 knd2 -> unifyKindConstraint Eq knd1 knd2 >> solve
        MkKindNeq knd1 knd2 -> unifyKindConstraint Neq knd1 knd2 >> solve


  
unifyTypeConstraint :: Ty -> Ty -> SolverM ()
unifyTypeConstraint (TyVar v1 _knd1) (TyVar v2 _knd2) = do 
  vars <- gets slvTyVars 
  case (M.lookup v1 vars, M.lookup v2 vars) of 
    (Just ty1, Just ty2) -> do
      addConstraint (MkTyEq ty1 ty2) 
    (Nothing, Just ty) -> addTyVar v1 ty
    (Just ty, Nothing) -> addTyVar v2 ty
    (Nothing,Nothing) -> return ()
unifyTypeConstraint (TyVar v _) ty = do
  vars <- gets slvTyVars
  case M.lookup v vars of 
    Nothing -> addTyVar v ty 
    Just ty' -> addConstraint (MkTyEq ty' ty) 
unifyTypeConstraint ty1 ty2@TyVar{} = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1 _) ty2@(TyDecl n2 args2 _) 
 | n1 /= n2 = throwError (ErrTyNeq ty1 ty2)
 | otherwise = do 
     addConstraintsArgs n1 args1 args2
unifyTypeConstraint (TyShift ty1 _) (TyShift ty2 _) = do 
  unifyTypeConstraint ty1 ty2
unifyTypeConstraint (TyCo ty1 _) (TyCo ty2 _) = do 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTyNeq ty1 ty2)

unifyKindConstraint :: ConstrTy -> Kind -> Kind -> SolverM ()
unifyKindConstraint Eq (MkKind p1) (MkKind p2)  = when (p1 /= p2) $ throwError (ErrKindMisMatch p1 p2)
unifyKindConstraint Neq (MkKind p1) (MkKind p2) = when (p1==p2)   $ throwError (ErrKindMisMatch p1 p2)

unifyKindConstraint Eq (MkKindVar v1) (MkKind pol) = do
  kndEnv <- gets slvKndVars
  case M.lookup v1 kndEnv of 
    Nothing -> addKndVar v1 pol
    Just pol' -> if pol == pol' then return () else throwError $ ErrKindMisMatch pol pol'
unifyKindConstraint Eq p@MkKind{} v@MkKindVar{} = unifyKindConstraint Eq v p
unifyKindConstraint Eq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- gets slvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just pol,Nothing) -> addKndVar v2 pol
    (Nothing, Just pol) -> addKndVar v1 pol
    (Just pol1, Just pol2) -> when (pol1 /= pol2) $ throwError (ErrKindMisMatch pol1 pol2)


unifyKindConstraint Neq v@MkKindVar{} (MkKind p) = unifyKindConstraint Eq v (MkKind $ flipPol p)
unifyKindConstraint Neq (MkKind p) v@MkKindVar{} = unifyKindConstraint Eq v (MkKind $ flipPol p)
unifyKindConstraint Neq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- gets slvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just pol,Nothing) -> addKndVar v2 (flipPol pol)
    (Nothing, Just pol) -> addKndVar v1 (flipPol pol)
    (Just pol1, Just pol2) -> when (pol1 == pol2) $ throwError (ErrKindMisMatch pol1 pol2)

