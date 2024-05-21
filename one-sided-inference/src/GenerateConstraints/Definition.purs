module GenerateConstraints.Definition (
  GenM,
  GenerateState,
  runGenM,
  addGenVar,
  getGenVars,
  freshTyVar,
  freshTyVarsDecl,
  addTyvar,
  addConstraint,
  addConstraintsXtor
) where 


----------------------
-- Constraint Monad --
---------------------

import GenerateConstraints.Errors (GenerateError(..))
import Constraints (ConstraintSet, Constr(..))
import Loc (Loc)
import Common (Variable, Typevar(..), VariantVar(..),Xtorname)
import Environment (Environment,getVars)
import Syntax.Typed.Types (Ty(..))
import Syntax.Kinded.Types (embedType)
import Syntax.Kinded.Terms (getType)
import Syntax.Kinded.Program (VarDecl(..))

import Prelude (bind, (<>), show,pure,(+),(<$>),(*>))
import Data.Map (Map,empty,fromFoldable,insert,union)
import Data.List (List(..),elem)
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
  constrSet :: ConstraintSet ,
  genTyvars :: List Typevar
}

initialGenState :: GenerateState 
initialGenState = MkGenState {varEnv:empty, tyVarCnt:0, constrSet:Nil, genTyvars:Nil }

type GenM a = ReaderT Environment (StateT GenerateState (Except GenerateError)) a 

runGenM :: forall a.Environment -> GenM a -> Either GenerateError (Tuple a (Tuple (List Typevar) ConstraintSet))
runGenM env m = case runExcept (runStateT (runReaderT m env) initialGenState) of
  Left err -> Left err 
  Right (Tuple x (MkGenState st)) ->  Right (Tuple x (Tuple st.genTyvars st.constrSet))

freshTyVar :: GenM Ty
freshTyVar = do 
  (Tuple cnt vars) <- gets (\(MkGenState st) -> Tuple st.tyVarCnt st.genTyvars)
  let (Tuple newVar newCnt) = getNewVar vars cnt 
  _ <- modify (\(MkGenState s) -> MkGenState s{tyVarCnt=newCnt, genTyvars=Cons newVar s.genTyvars}) 
  pure (TyVar newVar)
  where 
    getNewVar :: List Typevar -> Int -> (Tuple Typevar Int)
    getNewVar vars' i = let newVar = Typevar ("X" <> show i) in if newVar `elem` vars' then  getNewVar vars' (i+1) else Tuple newVar i

freshTyVarsDecl :: List VariantVar -> GenM (Tuple (List Ty) (Map Typevar Ty))
freshTyVarsDecl vars = do
  varL <- for vars (\(VariantVar v) -> do
    v' <- freshTyVar
    let varpair = Tuple v.variantVar v'
    pure (Tuple v' varpair))
  let newVars = fst <$> varL
  let newMap = fromFoldable (snd <$> varL)
  pure (Tuple newVars newMap)

addTyvar :: Typevar -> GenM Unit
addTyvar var = modify (\(MkGenState s) -> MkGenState s{genTyvars=Cons var s.genTyvars}) *> pure unit


-- modify environment
addConstraint :: Constr -> GenM Unit 
addConstraint ctr = do
  _ <- modify (\(MkGenState s) -> MkGenState s{constrSet=Cons ctr s.constrSet})
  pure unit

getGenVars :: GenM (Map Variable Ty)
getGenVars = do
  envVars <- getVars
  let envTys = (\(VarDecl var) -> embedType (getType var.varBody)) <$> envVars
  genVars <- gets (\(MkGenState s) -> s.varEnv)
  pure (union envTys genVars)

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
