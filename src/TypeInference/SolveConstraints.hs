module TypeInference.SolveConstraints where 

import TypeInference.Definition qualified as Df
import Typed.Types
import Typed.Syntax
import Common 
import Data.Map qualified as M
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Pretty () 

--
-- Solver Monad 
-- 
data SolverState = MkSolverState 
  { 
  slvTyVars :: !(M.Map TypeVar Ty), 
  slvKndVars :: !(M.Map KindVar Pol),
  remConstrs :: ![Df.Constraint]
}

initialSolverState :: [Df.Constraint] -> SolverState
initialSolverState = MkSolverState M.empty M.empty 

newtype SolverM a = MkSolveM { getSolveM :: StateT SolverState (Except String) a }
  deriving newtype (Functor, Applicative, Monad, MonadState SolverState, MonadError String)

runSolveM :: [Df.Constraint] -> SolverM a -> Either String (a,M.Map TypeVar Ty, M.Map KindVar Pol)
runSolveM constrs m = case runExcept (runStateT (getSolveM m) (initialSolverState constrs) ) of 
  Left err -> Left err 
  Right (x,st) -> Right (x,slvTyVars st, slvKndVars st)

runSolve :: [Df.Constraint] -> Either String ((),M.Map TypeVar Ty,M.Map KindVar Pol)
runSolve ctrs = runSolveM ctrs solve

solve :: SolverM () 
solve = do
  ctrs <- gets remConstrs
  case ctrs of 
    [] -> return ()
    (ctr1:ctrs') -> do 
      modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) ctrs')
      case ctr1 of 
        Df.MkTyEq ty1 ty2 -> do 
          unifyTypeConstraint ty1 ty2 
          solve
        Df.MkKindEq knd1 knd2 -> do 
          unifyKinds knd1 knd2 
          solve
        Df.MkFlipEq knd1 knd2 -> do
          unifyFlipKinds knd1 knd2
          solve
        Df.MkProdEq k1 k2 k3 -> do
          unifyProdKinds k1 k2 k3
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
  eqs <- gets remConstrs 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (Df.MkKindEq (MkKindVar v1) (MkKindVar v2) :eqs))

addVarNeq :: KindVar -> KindVar -> SolverM ()
addVarNeq v1 v2 = do 
  neqs <- gets remConstrs 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (Df.MkFlipEq (MkKindVar v1) (MkKindVar v2) : neqs) )

addTyEq :: Ty -> Ty -> SolverM () 
addTyEq ty1 ty2 = do 
  constrs <- gets remConstrs 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (Df.MkTyEq ty1 ty2 : constrs))

unifyKinds :: Kind -> Kind -> SolverM () 
unifyKinds (MkKind p1) (MkKind p2) = 
  if p1==p2 then return () else throwError ("cannot unify " <> show p1 <> " and " <> show p2)
unifyKinds (MkKindVar v) (MkKind p) = do 
  vars <- gets slvKndVars
  case M.lookup v vars of 
    Nothing -> addKndVar v p  
    Just p' -> if p == p' then return () else throwError ("cannot unify " <> show p <> " and " <> show p')
unifyKinds (MkKind p) (MkKindVar v) = unifyKinds (MkKindVar v) (MkKind p)
unifyKinds (MkKindVar v1) (MkKindVar v2) = do 
  vars <- gets slvKndVars 
  case (M.lookup v1 vars, M.lookup v2 vars) of 
    (Nothing, Nothing) -> addVarEq v1 v2
    (Nothing, Just p) -> addKndVar v1 p
    (Just p, Nothing) -> addKndVar v2 p
    (Just p1, Just p2) -> if p1 == p2 then return () else throwError ("canont unify " <> show p1 <> " and " <> show p2)

unifyFlipKinds :: Kind -> Kind -> SolverM () 
unifyFlipKinds (MkKind p1) k = unifyKinds (MkKind $ flipPol p1) k
unifyFlipKinds k (MkKind p2) = unifyKinds k (MkKind $ flipPol p2)
unifyFlipKinds (MkKindVar v1) (MkKindVar v2) = do
 vars <- gets slvKndVars 
 case (M.lookup v1 vars, M.lookup v2 vars) of 
   (Nothing,Nothing) -> addVarNeq v2 v2 
   (Nothing, Just p) -> addKndVar v1 (flipPol p)
   (Just p, Nothing) -> addKndVar v2 (flipPol p)
   (Just p1, Just p2) -> if p1 == flipPol p2 then return () else throwError ("cannot unify " <> show p1 <> " and " <> show (flipPol p2))

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
unifyTypeConstraint (TyDecl n1 args1 knd1) (TyDecl n2 args2 knd2) = 
  if n1 == n2 && length args1 == length args2 then do 
    forM_ (zip args1 args2) (uncurry addTyEq)
    unifyKinds knd1 knd2
  else throwError ("Cannot unify " <> n1 <> " and " <> n2 <> " with args " <> show args1 <> " and " <> show args2)
unifyTypeConstraint (TyShift ty1 knd1) (TyShift ty2 knd2) = do 
  unifyKinds knd1 knd2 
  unifyTypeConstraint ty1 ty2
unifyTypeConstraint (TyCo ty1 knd1) (TyCo ty2 knd2) = do 
  unifyKinds knd1 knd2 
  unifyTypeConstraint ty1 ty2 
unifyTypeConstraint ty1 ty2 = throwError ("cannot unify " <> show ty1 <> " and " <> show ty2)
