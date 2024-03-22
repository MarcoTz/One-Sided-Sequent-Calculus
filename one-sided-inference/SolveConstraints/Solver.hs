module SolveConstraints.Solver (
  solve
) where 

import SolveConstraints.Definition
import Constraints
import Syntax.Typed.Types
import Common
import Errors
import Embed.Definition
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
  let pv1 = MkPolVar v1 knd1
  let pv2 = MkPolVar v2 knd2
  case (M.lookup pv1 vars, M.lookup pv2 vars) of 
    (Just ty1, Just ty2) -> do
      addConstraint (MkTyEq ty1 ty2) 
    (Nothing, Just ty) -> addSlvTyVar pv1 ty
    (Just ty, Nothing) -> addSlvTyVar pv2 ty
    (Nothing,Nothing) -> return ()
unifyTypeConstraint (TyVar v knd) ty = do
  vars <- getSlvTyVars 
  let pv = MkPolVar v knd
  case M.lookup pv vars of 
    Nothing -> addSlvTyVar pv ty 
    Just ty' -> addConstraint (MkTyEq ty' ty) 
unifyTypeConstraint ty1 ty2@TyVar{} = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1 _) ty2@(TyDecl n2 args2 _) 
 | n1 /= n2 = throwError (ErrTypeNeq (embed ty1) (embed ty2) "unifyTypeConstraint (solver)")
 | otherwise = do 
     addConstraintsArgs n1 args1 args2
unifyTypeConstraint (TyShift ty1 knd1) (TyShift ty2 knd2) = do
  if knd1 == knd2 then unifyTypeConstraint ty1 ty2 else throwError (ErrKind ShouldEq (embed ty1) (embed ty2) "uniftTypeConstratint")
unifyTypeConstraint (TyCo ty1) (TyCo ty2) = do 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTypeNeq (embed ty1) (embed ty2) "unifypeConstraint (solver)")


genTy :: Pol -> Ty
genTy = TyVar (MkTypeVar "a") 

unifyKindConstraint :: ConstrTy -> Kind -> Kind -> SolverM ()
unifyKindConstraint Eq (MkKind p1) (MkKind p2)  = when (p1 /= p2) $ throwError (ErrKind ShouldEq (embed $ genTy p1) (embed $ genTy p2) "unifyKindConstraint (solver)")
unifyKindConstraint Neq (MkKind p1) (MkKind p2) = when (p1==p2)   $ throwError (ErrKind ShouldNeq (embed $ genTy p1) (embed $ genTy p2) "unifyTypeConstraint (solver)")

unifyKindConstraint Eq (MkKindVar v1) (MkKind pol) = do
  kndEnv <- getSlvKndVars 
  case M.lookup v1 kndEnv of 
    Nothing -> addSlvKndVar v1 pol
    Just pol' -> if pol == pol' then return () else throwError (ErrKind ShouldEq (embed $ genTy pol) (embed $ genTy pol') "unifyKindConstraint (solver)")
unifyKindConstraint Eq p@MkKind{} v@MkKindVar{} = unifyKindConstraint Eq v p
unifyKindConstraint Eq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- getSlvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just pol,Nothing) -> addSlvKndVar v2 pol
    (Nothing, Just pol) -> addSlvKndVar v1 pol
    (Just pol1, Just pol2) -> when (pol1 /= pol2) $ throwError (ErrKind ShouldEq (embed $ genTy pol1) (embed $ genTy pol2) "unifyKindConstraint (solver)")


unifyKindConstraint Neq v@MkKindVar{} (MkKind p) = unifyKindConstraint Eq v (MkKind $ flipPol p)
unifyKindConstraint Neq (MkKind p) v@MkKindVar{} = unifyKindConstraint Eq v (MkKind $ flipPol p)
unifyKindConstraint Neq (MkKindVar v1) (MkKindVar v2) = do
  kndEnv <- getSlvKndVars 
  case (M.lookup v1 kndEnv,M.lookup v2 kndEnv) of 
    (Nothing, Nothing) -> return ()
    (Just pol,Nothing) -> addSlvKndVar v2 (flipPol pol)
    (Nothing, Just pol) -> addSlvKndVar v1 (flipPol pol)
    (Just pol1, Just pol2) -> when (pol1 == pol2) $ throwError (ErrKind ShouldNeq (embed $ genTy pol1) (embed $ genTy pol2) "unifyKindConstraint (solver)")
