module SolveConstraints.Solver (
  solve
) where 

import SolveConstraints.Definition
import SolveConstraints.Errors
import Constraints
import Syntax.Typed.Types
import Common
import Embed.EmbedTyped ()

import Control.Monad
import Control.Monad.Except
import Data.Map qualified as M

solve :: SolverM () 
solve = do
  mctr <- getNextConstr
  case mctr of 
    Nothing -> return ()
    Just (MkTyEq ty1 ty2) -> unifyTypeConstraint ty1 ty2  >> solve
    Just (MkKindEq knd1 knd2) -> unifyKindConstraint Eq knd1 knd2 >> solve
    Just (MkKindNeq knd1 knd2) -> unifyKindConstraint Neq knd1 knd2 >> solve


  
unifyTypeConstraint :: Ty -> Ty -> SolverM ()
unifyTypeConstraint (TyVar v1 knd1) (TyVar v2 knd2) = do 
  vars <- getSlvTyVars 
  case (M.lookup v1 vars, M.lookup v2 vars) of 
    (Just ty1, Just ty2) -> do
      addConstraint (MkTyEq ty1 ty2) 
      addConstraint (MkKindEq knd1 (getKind ty1))
      addConstraint (MkKindEq knd2 (getKind ty2))
    (Nothing, Just ty) -> addSlvTyVar v1 ty
    (Just ty, Nothing) -> addSlvTyVar v2 ty
    (Nothing,Nothing) -> return ()
unifyTypeConstraint (TyVar v knd) ty = do
  addConstraint (MkKindEq knd (getKind ty))
  vars <- getSlvTyVars 
  case M.lookup v vars of 
    Nothing -> addSlvTyVar v ty 
    Just ty' -> addConstraint (MkTyEq ty' ty) 
unifyTypeConstraint ty1 ty2@TyVar{} = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1 _) ty2@(TyDecl n2 args2 _) 
 | n1 /= n2 = throwError (ErrTyNeq ty1 ty2)
 | otherwise = do 
     addConstraintsArgs n1 args1 args2
unifyTypeConstraint (TyShift ty1 knd1) (TyShift ty2 knd2) = do
  if knd1 == knd2 then unifyTypeConstraint ty1 ty2 else throwError (ErrTypeKindNeq ty1 ty2)
unifyTypeConstraint (TyCo ty1) (TyCo ty2) = do 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTyNeq ty1 ty2)

unifyKindConstraint :: ConstrTy -> Kind -> Kind -> SolverM ()
unifyKindConstraint Eq (MkKind p1) (MkKind p2)  = when (p1 /= p2) $ throwError (ErrKindNeq (MkKind p1) (MkKind p2))
unifyKindConstraint Neq (MkKind p1) (MkKind p2) = when (p1==p2)   $ throwError (ErrKindNeq (MkKind p1) (MkKind p2))

unifyKindConstraint Eq (MkKindVar v1) (MkKind eo) = do
  kndEnv <- getSlvKndVars 
  case M.lookup v1 kndEnv of 
    Nothing -> addSlvKndVar v1 eo
    Just eo' -> if eo == eo' then return () else throwError (ErrKindNeq (MkKind eo) (MkKind eo'))
unifyKindConstraint Eq p@MkKind{} v@MkKindVar{} = unifyKindConstraint Eq v p
unifyKindConstraint Eq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- getSlvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just eo,Nothing) -> addSlvKndVar v2 eo
    (Nothing, Just eo) -> addSlvKndVar v1 eo
    (Just eo1, Just eo2) -> when (eo1 /= eo2) $ throwError (ErrKindNeq (MkKind eo1) (MkKind eo2))


unifyKindConstraint Neq v@MkKindVar{} (MkKind p) = unifyKindConstraint Eq v (MkKind $ shiftEvalOrder p)
unifyKindConstraint Neq (MkKind p) v@MkKindVar{} = unifyKindConstraint Eq v (MkKind $ shiftEvalOrder p)
unifyKindConstraint Neq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- getSlvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just pol,Nothing) -> addSlvKndVar v2 (shiftEvalOrder pol)
    (Nothing, Just pol) -> addSlvKndVar v1 (shiftEvalOrder pol)
    (Just pol1, Just pol2) -> when (pol1 == pol2) $ throwError (ErrKindNeq (MkKind pol1) (MkKind pol2))
