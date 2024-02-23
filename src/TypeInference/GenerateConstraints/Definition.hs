module TypeInference.GenerateConstraints.Definition where 

import TypeInference.Constraints
import Syntax.Typed.Types
import Syntax.Typed.Program
import Common
import Errors

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as M
import Data.Bifunctor (second)

---
--- Constraint Monad
--- 


data GenerateState = MkGenState{
  varEnv :: !(M.Map Variable Ty),
  tyVarCnt :: !Int,
  declEnv :: !Program,
  constrSet :: !ConstraintSet 
}

initialGenState :: Program -> GenerateState 
initialGenState prog = MkGenState M.empty 0 prog (MkConstraintSet [])


newtype GenM a = GenM { getGenM :: StateT GenerateState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState GenerateState, MonadError Error)

runGenM :: Program -> GenM a -> Either Error (a, ConstraintSet)
runGenM prog m = case runExcept (runStateT (getGenM m) (initialGenState prog)) of
  Left err -> Left err 
  Right (x, st) ->  Right (x,constrSet st)

-- Fresh Variables 
freshTyVar :: GenM TypeVar
freshTyVar = do 
  cnt <- gets tyVarCnt
  let newVar = "X" <> show cnt
  modify (\s -> MkGenState (varEnv s) (cnt+1) (declEnv s) (constrSet s))
  return newVar


freshTyVarsDecl :: [(Variable,Pol)] -> GenM ([Variable],M.Map Variable Ty) 
freshTyVarsDecl vars = do
  varL <- forM vars (\(v,_) -> do
    v' <- freshTyVar
    return (v,v'))
  let newVars = snd <$> varL
  let newMap = M.fromList (second TyVar <$> varL )
  return (newVars, newMap)

-- modify environment
insertConstraint :: Constraint -> GenM () 
insertConstraint ctr = modify (\s -> MkGenState (varEnv s) (tyVarCnt s) (declEnv s) (addConstraint ctr (constrSet s)))

addVar :: Variable -> Ty -> GenM ()
addVar v ty = do 
  vars <- gets varEnv
  modify (\s -> MkGenState (M.insert v ty vars) (tyVarCnt s) (declEnv s) (constrSet s))

findDataDecl :: XtorName -> GenM (Maybe (DataDecl,XtorSig))
findDataDecl nm = do
  prog <- gets declEnv 
  return $ checkDecl nm (progDecls prog)
  where
    checkDecl :: XtorName -> [DataDecl] -> Maybe (DataDecl,XtorSig)
    checkDecl _ [] = Nothing
    checkDecl n (d@(MkDataDecl _ _ _ xtors):dcs) = 
      case checkXtor n xtors of 
        Nothing -> checkDecl n dcs
        Just sig -> Just (d,sig)

    checkXtor :: XtorName -> [XtorSig] -> Maybe XtorSig
    checkXtor _ [] = Nothing
    checkXtor xtn (xt:xts) = if sigName xt == xtn then Just xt else checkXtor nm xts


addConstraintsXtor :: XtorName -> [Ty] -> [Ty] -> GenM () 
addConstraintsXtor _ [] [] = return ()
addConstraintsXtor xt _ [] = throwError (ErrArityXtor xt)
addConstraintsXtor xt [] _ = throwError (ErrArityXtor xt)
addConstraintsXtor xt (ty1:tys1) (ty2:tys2) = do 
  insertConstraint (MkTyEq ty1 ty2)
  addConstraintsXtor xt tys1 tys2
