module GenerateConstraints.Definition (
  GenM,
  GenerateState,
  runGenM,
  addGenVar,
  getGenVars,
  freshKVar,
  freshTyVar,
  freshTyVarsDecl,
  addConstraint,
  addConstraintsXtor
) where 


----------------------
-- Constraint Monad --
---------------------

import GenerateConstraints.Errors (GenerateError(..))
import Constraints (ConstraintSet, Constr(..))
import Loc (Loc)
import Common (Variable, Typevar(..), VariantVar(..),Kind(..),Kindvar(..), Xtorname)
import Environment (Environment)
import Syntax.Typed.Types (Ty(..))

import Prelude (bind, (<>), show,pure,(+),(<$>))
import Data.Map (Map,empty,fromFoldable,insert)
import Data.List (List(..))
import Data.Tuple (Tuple(..), fst,snd)
import Data.Either (Either(..))
import Data.Unit (Unit,unit)
import Data.Traversable (for)
import Control.Monad.Reader (ReaderT, runReaderT) 
import Control.Monad.State (StateT, runStateT, gets, modify)
import Control.Monad.Except (Except, runExcept, throwError)

data GenerateState = MkGenState{
  varEnv :: (Map Variable Ty),
  tyVarCnt :: Int,
  kVarCnt :: Int,
  constrSet :: ConstraintSet 
}

initialGenState :: GenerateState 
initialGenState = MkGenState {varEnv:empty, tyVarCnt:0, kVarCnt:0, constrSet:Nil}

type GenM a = ReaderT Environment (StateT GenerateState (Except GenerateError)) a 

runGenM :: forall a.Environment -> GenM a -> Either GenerateError (Tuple a ConstraintSet)
runGenM env m = case runExcept (runStateT (runReaderT m env) initialGenState) of
  Left err -> Left err 
  Right (Tuple x (MkGenState st)) ->  Right (Tuple x st.constrSet)

-- Fresh Variables 
freshTyVar :: GenM Ty
freshTyVar = do 
  cnt <- gets (\(MkGenState st) -> st.tyVarCnt)
  let newVar = Typevar {unTypevar:"X" <> show cnt}
  _ <- modify (\(MkGenState s) -> MkGenState s{tyVarCnt=cnt+1}) 
  pure (TyVar newVar)

freshTyVarsDecl :: List VariantVar -> GenM (Tuple (List Ty) (Map Typevar Ty))
freshTyVarsDecl vars = do
  varL <- for vars (\(VariantVar v) -> do
    v' <- freshTyVar
    let varpair = Tuple v.variantVar v'
    pure (Tuple v' varpair))
  let newVars = fst <$> varL
  let newMap = fromFoldable (snd <$> varL)
  pure (Tuple newVars newMap)

freshKVar :: GenM Kind
freshKVar = do 
  cnt <- gets (\(MkGenState s) -> s.kVarCnt )
  let newVar = Kindvar {unKindvar:"k" <> show cnt}
  _ <- modify (\(MkGenState s) -> MkGenState s{kVarCnt=cnt+1})
  pure (MkKindVar newVar)

-- modify environment
addConstraint :: Constr -> GenM Unit 
addConstraint ctr = do
  _ <- modify (\(MkGenState s) -> MkGenState s{constrSet=Cons ctr s.constrSet})
  pure unit

getGenVars :: GenM (Map Variable Ty)
getGenVars = gets (\(MkGenState s) -> s.varEnv)

addGenVar :: Variable -> Ty -> GenM Unit
addGenVar v ty = do 
  vars <- gets (\(MkGenState s) -> s.varEnv)
  _ <- modify (\(MkGenState s) -> MkGenState s{varEnv=insert v ty vars})
  pure unit


addConstraintsXtor :: Loc -> Xtorname -> List Ty -> List Ty -> GenM Unit
addConstraintsXtor _ _ Nil Nil = pure unit
addConstraintsXtor loc xt _ Nil = throwError (ErrXtorArity loc xt)
addConstraintsXtor loc xt Nil _ = throwError (ErrXtorArity loc xt)
addConstraintsXtor loc xt (Cons ty1 tys1) (Cons ty2 tys2) = do 
  _ <- addConstraint (MkTyEq ty1 ty2)
  _ <- addConstraintsXtor loc xt tys1 tys2
  pure unit
