module SolveConstraints.Solver (
  solve
) where 

import SolveConstraints.Definition
import SolveConstraints.Errors
import Constraints
import Syntax.Typed.Types
import Embed.EmbedTyped ()

import Control.Monad.Except
import Data.Map qualified as M

solve :: SolverM () 
solve = do
  mctr <- getNextConstr
  case mctr of 
    Nothing -> return ()
    Just (MkTyEq ty1 ty2) -> unifyTypeConstraint ty1 ty2  >> solve
    Just MkKindEq{} -> error "should not happen" 
    Just MkKindNeq{} -> error "should not happen"


  
unifyTypeConstraint :: Ty -> Ty -> SolverM ()
unifyTypeConstraint (TyVar v1) (TyVar v2) = do 
  vars <- getSlvTyVars 
  case (M.lookup v1 vars, M.lookup v2 vars) of 
    (Just ty1, Just ty2) -> do
      addConstraint (MkTyEq ty1 ty2) 
    (Nothing, Just ty) -> addSlvTyVar v1 ty
    (Just ty, Nothing) -> addSlvTyVar v2 ty
    (Nothing,Nothing) -> return ()
unifyTypeConstraint (TyVar v) ty = do
  vars <- getSlvTyVars 
  case M.lookup v vars of 
    Nothing -> addSlvTyVar v ty 
    Just ty' -> addConstraint (MkTyEq ty' ty) 
unifyTypeConstraint ty1 ty2@TyVar{} = unifyTypeConstraint ty2 ty1
unifyTypeConstraint ty1@(TyDecl n1 args1) ty2@(TyDecl n2 args2) 
 | n1 /= n2 = throwError (ErrTyNeq ty1 ty2)
 | otherwise = do 
     addConstraintsArgs n1 args1 args2
unifyTypeConstraint (TyShift ty1) (TyShift ty2) = unifyTypeConstraint ty1 ty2 
unifyTypeConstraint (TyCo ty1) (TyCo ty2) = do 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError (ErrTyNeq ty1 ty2)
