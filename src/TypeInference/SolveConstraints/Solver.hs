module TypeInference.SolveConstraints.Solver where 

import TypeInference.SolveConstraints.Definition
import TypeInference.Constraints
import Syntax.Typed.Types
import Common
import Errors
import Embed.Definition
import Embed.EmbedTyped ()

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
unifyTypeConstraint (TyVar v1 knd1) (TyVar v2 knd2) = do 
  vars <- gets slvTyVars 
  let pv1 = MkPolVar v1 knd1
  let pv2 = MkPolVar v2 knd2
  case (M.lookup pv1 vars, M.lookup pv2 vars) of 
    (Just ty1, Just ty2) -> do
      addConstraint (MkTyEq ty1 ty2) 
    (Nothing, Just ty) -> addTyVar pv1 ty
    (Just ty, Nothing) -> addTyVar pv2 ty
    (Nothing,Nothing) -> return ()
unifyTypeConstraint (TyVar v knd) ty = do
  vars <- gets slvTyVars
  let pv = MkPolVar v knd
  case M.lookup pv vars of 
    Nothing -> addTyVar pv ty 
    Just ty' -> addConstraint (MkTyEq ty' ty) 
unifyTypeConstraint ty1 ty2@TyVar{} = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1 _) ty2@(TyDecl n2 args2 _) 
 | n1 /= n2 = throwError (ErrTypeNeq (embed ty1) (embed ty2) WhereSolve)
 | otherwise = do 
     addConstraintsArgs n1 args1 args2
unifyTypeConstraint (TyShift ty1) (TyShift ty2) = do 
  unifyTypeConstraint ty1 ty2
unifyTypeConstraint (TyCo ty1) (TyCo ty2) = do 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTypeNeq (embed ty1) (embed ty2) WhereSolve)

unifyKindConstraint :: ConstrTy -> Kind -> Kind -> SolverM ()
unifyKindConstraint Eq (MkKind p1) (MkKind p2)  = when (p1 /= p2) $ throwError (ErrKind (MkKind p1) (MkKind p2) ShouldEq WhereSolve)
unifyKindConstraint Neq (MkKind p1) (MkKind p2) = when (p1==p2)   $ throwError (ErrKind (MkKind p1) (MkKind p2) ShouldNeq WhereSolve)

unifyKindConstraint Eq (MkKindVar v1) (MkKind pol) = do
  kndEnv <- gets slvKndVars
  case M.lookup v1 kndEnv of 
    Nothing -> addKndVar v1 pol
    Just pol' -> if pol == pol' then return () else throwError (ErrKind (MkKind pol) (MkKind pol') ShouldEq WhereSolve)
unifyKindConstraint Eq p@MkKind{} v@MkKindVar{} = unifyKindConstraint Eq v p
unifyKindConstraint Eq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- gets slvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just pol,Nothing) -> addKndVar v2 pol
    (Nothing, Just pol) -> addKndVar v1 pol
    (Just pol1, Just pol2) -> when (pol1 /= pol2) $ throwError (ErrKind (MkKind pol1) (MkKind pol2) ShouldEq WhereSolve)


unifyKindConstraint Neq v@MkKindVar{} (MkKind p) = unifyKindConstraint Eq v (MkKind $ flipPol p)
unifyKindConstraint Neq (MkKind p) v@MkKindVar{} = unifyKindConstraint Eq v (MkKind $ flipPol p)
unifyKindConstraint Neq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- gets slvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just pol,Nothing) -> addKndVar v2 (flipPol pol)
    (Nothing, Just pol) -> addKndVar v1 (flipPol pol)
    (Just pol1, Just pol2) -> when (pol1 == pol2) $ throwError (ErrKind (MkKind pol1) (MkKind pol2) ShouldNeq WhereSolve)
