module TypeInference.Definition where 

import Typed.Types
import Common

import Data.Map qualified as M
import Control.Monad.Except
import Control.Monad.State


---
--- Constraint Monad
--- 
data Constraint = 
  MkTyEq !Ty !Ty
  | MkKindEq !Kind !Kind
  | MkFlipEq !Kind !Kind
  | MkProdEq !Pol !Kind !Kind


data GenerateState = MkGenState{
  varEnv :: !(M.Map Variable (Ty,Kind)),
  tyVarEnv :: !(M.Map TypeVar Kind),
  tyVarCnt :: !Int,
  kndVarCnt :: !Int,
  declEnv :: ![Decl],
  constrSet :: ![Constraint]
}

initialGenState :: GenerateState
initialGenState = MkGenState M.empty M.empty 0 0 [] []

newtype GenM a = MkGenM { getGenM :: ExceptT String (State GenerateState) a }
  deriving newtype (Functor, Applicative, Monad, MonadState GenerateState, MonadError String)


-- Fresh Variables 
freshTyVar :: GenM TypeVar
freshTyVar = do 
  cnt <- gets tyVarCnt
  let newVar = "X" <> show cnt
  modify (\s -> MkGenState (varEnv s) (tyVarEnv s) (cnt+1) (kndVarCnt s) (declEnv s) (constrSet s))
  return newVar

freshKndVar :: GenM KindVar
freshKndVar = do 
  cnt <- gets kndVarCnt 
  let newVar = "K" <> show cnt
  modify (\s -> MkGenState (varEnv s) (tyVarEnv s) (tyVarCnt s) (cnt+1) (declEnv s) (constrSet s))
  return newVar


-- modify environment
addConstraint :: Constraint -> GenM () 
addConstraint ctr = modify (\s -> MkGenState (varEnv s) (tyVarEnv s) (tyVarCnt s) (kndVarCnt s) (declEnv s) (ctr:constrSet s))

addVar :: Variable -> (Ty,Kind) -> GenM ()
addVar v (ty,knd) = do 
  vars <- gets varEnv
  modify (\s -> MkGenState (M.insert v (ty,knd) vars) (tyVarEnv s) (tyVarCnt s) (kndVarCnt s) (declEnv s) (constrSet s))

addTyVar :: TypeVar -> Kind -> GenM ()
addTyVar tyv knd = do
  tyVars <- gets tyVarEnv 
  modify (\s -> MkGenState (varEnv s) (M.insert tyv knd tyVars) (tyVarCnt s) (kndVarCnt s) (declEnv s) (constrSet s))

findDataDecl :: XtorName -> GenM (Maybe Decl)
findDataDecl nm = do
  decls <- gets declEnv 
  return $ checkDecl nm decls 
  where
    checkDecl :: XtorName -> [Decl] -> Maybe Decl
    checkDecl _ [] = Nothing
    checkDecl n (d@(MkDataDecl _ _ _ xtors):dcs) = if any (\sig -> sigName sig == n) xtors then Just d else checkDecl n dcs
    checkDecl n (_:dcs) = checkDecl n dcs


--
-- Solver Monad 
-- 
data SolverState = MkSolverState { slvTyVars :: !(M.Map TypeVar Ty), slvKndVars :: !(M.Map KindVar Pol), slvVarEq :: ![(KindVar, KindVar)], slvVarNeq :: ![(KindVar,KindVar)]}

initialSolverState :: SolverState
initialSolverState = MkSolverState M.empty M.empty [] []

newtype SolverM a = MkSolveM { getSolveM :: ExceptT String (State SolverState) a }
  deriving newtype (Functor, Applicative, Monad, MonadState SolverState, MonadError String)
