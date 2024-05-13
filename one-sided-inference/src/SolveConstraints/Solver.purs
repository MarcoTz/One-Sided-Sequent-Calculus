module SolveConstraints.Solver (
  solve
) where 


import SolveConstraints.Definition (SolverM,getNextConstr,getSlvTyVars,addConstraint, addSlvTyVar, addConstraintsArgs) 
import SolveConstraints.Errors (SolverError(..))
import Constraints (Constr(..))
import Loc (defaultLoc)
import Syntax.Typed.Types (Ty(..))

import Prelude (bind,pure,(==), (/=), otherwise) 
import Data.Unit (Unit,unit)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Map (lookup)
import Control.Monad.Except (throwError)

solve :: SolverM Unit
solve = do
  mctr <- getNextConstr
  case mctr of 
    Nothing -> pure unit
    Just (MkTyEq ty1 ty2) -> do
      _ <- unifyTypeConstraint ty1 ty2 
      solve
    Just (MkKindEq _ _) -> throwError (ErrOther defaultLoc "should not happen" )
    Just (MkKindNeq _ _) -> throwError (ErrOther defaultLoc "should not happen")


  
unifyTypeConstraint :: Ty -> Ty -> SolverM Unit
unifyTypeConstraint (TyForall _ ty1) ty2 = unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 (TyForall _ ty2) = unifyTypeConstraint ty1 ty2
unifyTypeConstraint (TyVar v1) (TyVar v2) | v1 == v2 = pure unit
unifyTypeConstraint (TyVar v1) (TyVar v2) = do 
  vars <- getSlvTyVars 
  case (Tuple (lookup v1 vars) (lookup v2 vars)) of 
    (Tuple (Just ty1) (Just ty2)) -> do
      _ <- addConstraint (MkTyEq ty1 ty2) 
      pure unit
    (Tuple Nothing (Just ty)) -> addSlvTyVar v1 ty
    (Tuple (Just ty) Nothing) -> addSlvTyVar v2 ty
    (Tuple Nothing Nothing) -> addSlvTyVar v2 (TyVar v1) 
unifyTypeConstraint (TyVar v) ty = do
  vars <- getSlvTyVars 
  case lookup v vars of 
    Nothing -> addSlvTyVar v ty 
    Just ty' -> addConstraint (MkTyEq ty' ty) 
unifyTypeConstraint ty1 ty2@(TyVar _) = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1) ty2@(TyDecl n2 args2) 
 | n1 /= n2 = throwError (ErrTyNeq ty1 ty2)
 | otherwise = do 
     _ <- addConstraintsArgs n1 args1 args2
     pure unit
unifyTypeConstraint (TyShift ty1) (TyShift ty2) = unifyTypeConstraint ty1 ty2 
unifyTypeConstraint (TyCo ty1) (TyCo ty2) = do 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTyNeq ty1 ty2)
