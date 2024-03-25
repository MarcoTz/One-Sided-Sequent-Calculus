module GenerateConstraints.Definition (
  GenM,
  runGenM,
  addGenVar,
  getGenVars,
  freshTyVar,
  freshTyVarsDecl,
  addConstraint,
  addConstraintsXtor,
  GenerateError (..)
) where 

import GenerateConstraints.Errors
import Constraints
import Syntax.Typed.Types
import Common
import Loc
import Environment
import Pretty.Common ()
import Pretty.Typed ()

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

newtype GenM a = GenM { getGenM :: ReaderT Environment (StateT GenerateState (Except GenerateError)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState GenerateState, MonadError GenerateError, MonadReader Environment)

runGenM :: Environment -> GenM a -> Either GenerateError (a, ConstraintSet)
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

getGenVars :: GenM (M.Map Variable Ty)
getGenVars = gets varEnv

addGenVar :: Variable -> Ty -> GenM ()
addGenVar v ty = do 
  vars <- gets varEnv
  modify (\s -> MkGenState (M.insert v ty vars) (kVarCnt s) (tyVarCnt s) (constrSet s))


addConstraintsXtor :: Loc -> XtorName -> [Ty] -> [Ty] -> GenM () 
addConstraintsXtor _ _ [] [] = return ()
addConstraintsXtor loc xt _ [] = throwError (ErrXtorArity loc xt)
addConstraintsXtor loc xt [] _ = throwError (ErrXtorArity loc xt)
addConstraintsXtor loc xt (ty1:tys1) (ty2:tys2) = do 
  addConstraint (MkTyEq ty1 ty2)
  addConstraintsXtor loc xt tys1 tys2
