module TypeInference.GenerateConstraints.Definition where 

import TypeInference.Constraints
import Syntax.Typed.Types
import Common
import Errors
import Environment

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M

----------------------
-- Constraint Monad --
---------------------


data GenerateState = MkGenState{
  varEnv :: !(M.Map Variable Ty),
  tyVarCnt :: !Int,
  kVarCnt :: !Int,
  constrSet :: !ConstraintSet 
}

initialGenState :: GenerateState 
initialGenState = MkGenState M.empty 0 0 (MkConstraintSet [])


newtype GenM a = GenM { getGenM :: ReaderT Environment (StateT GenerateState (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState GenerateState, MonadError Error, MonadReader Environment)

runGenM :: Environment -> GenM a -> Either Error (a, ConstraintSet)
runGenM env m = case runExcept (runStateT (runReaderT (getGenM m) env) initialGenState) of
  Left err -> Left err 
  Right (x, st) ->  Right (x,constrSet st)

-- Fresh Variables 
freshTyVar :: Pol-> GenM Ty
freshTyVar pol = do 
  cnt <- gets tyVarCnt
  let newVar = MkTypeVar ("X" <> show cnt)
  modify (\s -> MkGenState (varEnv s) (kVarCnt s) (cnt+1) (constrSet s))
  return (TyVar newVar pol)

freshKVar :: GenM Kind
freshKVar = do 
  cnt <- gets kVarCnt
  let newVar = MkKVar ("k" <> show cnt)
  modify (\s -> MkGenState (varEnv s) (tyVarCnt s) (cnt+1) (constrSet s))
  return (MkKindVar newVar)


freshTyVarsDecl :: [PolVar] -> GenM ([Ty],M.Map PolVar Ty) 
freshTyVarsDecl vars = do
  varL <- forM vars (\(MkPolVar v p) -> do
    v' <- freshTyVar p
    let varpair = (MkPolVar v p,v')
    return (v',varpair))
  let newVars = fst <$> varL
  let newMap = M.fromList (snd <$> varL)
  return (newVars, newMap)

-- modify environment
addConstraint :: Constraint -> GenM () 
addConstraint ctr = modify (\s -> MkGenState (varEnv s) (kVarCnt s) (tyVarCnt s) (insertConstraint ctr (constrSet s)))

addVar :: Variable -> Ty -> GenM ()
addVar v ty = do 
  vars <- gets varEnv
  modify (\s -> MkGenState (M.insert v ty vars) (kVarCnt s) (tyVarCnt s) (constrSet s))


addConstraintsXtor :: XtorName -> [Ty] -> [Ty] -> GenM () 
addConstraintsXtor _ [] [] = return ()
addConstraintsXtor xt _ [] = throwError (ErrXtorArity xt "addConstraintsXtor (generate constraints)")
addConstraintsXtor xt [] _ = throwError (ErrXtorArity xt "addConstraintsXtor (generate constraints)")
addConstraintsXtor xt (ty1:tys1) (ty2:tys2) = do 
  addConstraint (MkTyEq ty1 ty2)
  addConstraintsXtor xt tys1 tys2
