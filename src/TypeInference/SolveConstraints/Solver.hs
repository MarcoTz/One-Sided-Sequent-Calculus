module TypeInference.SolveConstraints.Solver where 

import TypeInference.SolveConstraints.Definition
import TypeInference.Constraints
import Syntax.Typed.Types
import Syntax.Typed.Terms
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
      modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet ctrs'))
      case ctr1 of 
        MkTyEq ty1 ty2 -> do 
          unifyTypeConstraint ty1 ty2 
          solve
        MkKindEq knd1 knd2 -> do 
          unifyKinds knd1 knd2 
          solve
        MkFlipEq knd1 knd2 -> do
          unifyFlipKinds knd1 knd2
          solve

addTyVar :: Variable -> Ty -> SolverM ()
addTyVar v ty = do 
  vars <- gets slvTyVars
  modify (\s -> MkSolverState (M.insert v ty vars) (slvKndVars s) (remConstrs s))

addKndVar :: KindVar -> Pol -> SolverM () 
addKndVar v p = do 
  vars <- gets slvKndVars 
  modify (\s -> MkSolverState (slvTyVars s) (M.insert v p vars) (remConstrs s))

addVarEq :: KindVar -> KindVar -> SolverM () 
addVarEq v1 v2 = do 
  (MkConstraintSet eqs) <- gets remConstrs 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet (MkKindEq (MkKindVar v1) (MkKindVar v2) :eqs)))

addVarNeq :: KindVar -> KindVar -> SolverM ()
addVarNeq v1 v2 = do 
  (MkConstraintSet neqs) <- gets remConstrs 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet (MkFlipEq (MkKindVar v1) (MkKindVar v2) : neqs)))

addTyEq :: Ty -> Ty -> SolverM () 
addTyEq ty1 ty2 = do 
  (MkConstraintSet constrs) <- gets remConstrs 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (MkConstraintSet (MkTyEq ty1 ty2 : constrs)))

unifyKinds :: Kind -> Kind -> SolverM () 
unifyKinds (MkKind p1) (MkKind p2) = 
  if p1==p2 then return () else throwError (ErrKindMisMatch p1 p2)
unifyKinds (MkKindVar v) (MkKind p) = do 
  vars <- gets slvKndVars
  case M.lookup v vars of 
    Nothing -> addKndVar v p  
    Just p' -> if p == p' then return () else throwError (ErrKindMisMatch p p')
unifyKinds (MkKind p) (MkKindVar v) = unifyKinds (MkKindVar v) (MkKind p)
unifyKinds (MkKindVar v1) (MkKindVar v2) = do 
  vars <- gets slvKndVars 
  case (M.lookup v1 vars, M.lookup v2 vars) of 
    (Nothing, Nothing) -> addVarEq v1 v2
    (Nothing, Just p) -> addKndVar v1 p
    (Just p, Nothing) -> addKndVar v2 p
    (Just p1, Just p2) -> if p1 == p2 then return () else throwError (ErrKindMisMatch p1 p2)

unifyFlipKinds :: Kind -> Kind -> SolverM () 
unifyFlipKinds (MkKind p1) k = unifyKinds (MkKind $ flipPol p1) k
unifyFlipKinds k (MkKind p2) = unifyKinds k (MkKind $ flipPol p2)
unifyFlipKinds (MkKindVar v1) (MkKindVar v2) = do
 vars <- gets slvKndVars 
 case (M.lookup v1 vars, M.lookup v2 vars) of 
   (Nothing,Nothing) -> addVarNeq v2 v2 
   (Nothing, Just p) -> addKndVar v1 (flipPol p)
   (Just p, Nothing) -> addKndVar v2 (flipPol p)
   (Just p1, Just p2) -> if p1 == flipPol p2 then return () else throwError (ErrKindMisMatch p1 (flipPol p2))

unifyProdKinds :: Pol -> Kind -> Kind -> SolverM ()
unifyProdKinds Pos k2 k3 = unifyKinds k2 k3 
unifyProdKinds Neg k2 k3 = unifyFlipKinds k2 k3
  
unifyTypeConstraint :: Ty -> Ty -> SolverM ()
unifyTypeConstraint (TyVar v knd) ty = do
  unifyKinds knd (getKind ty)
  vars <- gets slvTyVars
  case M.lookup v vars of 
    Nothing -> addTyVar v ty 
    Just ty' -> addTyEq ty' ty
unifyTypeConstraint ty1 ty2@TyVar{} = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1 knd1) ty2@(TyDecl n2 args2 knd2) 
 | n1 /= n2 = throwError (ErrTyNeq ty1 ty2)
 | otherwise = do 
     addConstraintsArgs n1 args1 args2
     unifyKinds knd1 knd2

unifyTypeConstraint (TyShift ty1 knd1) (TyShift ty2 knd2) = do 
  unifyKinds knd1 knd2 
  unifyTypeConstraint ty1 ty2
unifyTypeConstraint (TyCo ty1 knd1) (TyCo ty2 knd2) = do 
  unifyKinds knd1 knd2 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTyNeq ty1 ty2)
