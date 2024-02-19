module TypeInference.SolveConstraints where 

import TypeInference.Definition
import Typed.Types
import Common 
import Data.Map qualified as M
import Control.Monad.Except
import Control.Monad.State
import Pretty () 

data SolverState = MkSolverState { slvTyVars :: !(M.Map TypeVar Ty), slvKndVars :: !(M.Map KindVar Pol), slvVarEq :: ![(KindVar, KindVar)], slvVarNeq :: ![(KindVar,KindVar)]}

initialSolverState :: SolverState
initialSolverState = MkSolverState M.empty M.empty [] []

newtype SolverM a = MkSolveM { getSolveM :: ExceptT String (State SolverState) a }
  deriving newtype (Functor, Applicative, Monad, MonadState SolverState, MonadError String)



solve :: [Constraint] -> SolverM () 
solve [] = return () 
solve (ctr1:ctrs) = 
  case ctr1 of 
    MkTyEq ty1 ty2 -> do 
      unifyTypeConstraint ty1 ty2 
      solve ctrs
      error "not implemented" 
    MkKindEq knd1 knd2 -> do 
      unifyKinds knd1 knd2 
      solve ctrs
    MkFlipEq knd1 knd2 -> do
      unifyFlipKinds knd1 knd2
      solve ctrs
--  | MkProdEq !Kind !Kind !Kind
    MkProdEq k1 k2 k3 -> do
      unifyProdKinds k1 k2 k3
      solve ctrs


addKndVar :: KindVar -> Pol -> SolverM () 
addKndVar v p = do 
  vars <- gets slvKndVars 
  modify (\s -> MkSolverState (slvTyVars s) (M.insert v p vars) (slvVarEq s) (slvVarNeq s))

addVarEq :: KindVar -> KindVar -> SolverM () 
addVarEq v1 v2 = do 
  eqs <- gets slvVarEq 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) ((v1,v2):eqs) (slvVarNeq s))

addVarNeq :: KindVar -> KindVar -> SolverM ()
addVarNeq v1 v2 = do 
  neqs <- gets slvVarNeq 
  modify (\s -> MkSolverState (slvTyVars s) (slvKndVars s) (slvVarEq s) ((v1,v2):neqs))

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
unifyTypeConstraint _ _ = return ()
