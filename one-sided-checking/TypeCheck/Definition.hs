module TypeCheck.Definition (
  runCheckM,
  CheckM,
  getCheckerVars,
  getCheckerTyVars,
  addCheckerVar,
  addCheckerTyVar,
  withCheckerVars,
  CheckerError (..)
) where 

import Errors 
import Loc
import Environment
import Common
import Syntax.Typed.Types qualified as T 
import Syntax.Typed.Terms qualified as T
import Syntax.Desugared.Terms qualified as D 
import Pretty.Typed () 
import Pretty.Desugared ()

import Control.Monad.Except 
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M
import Data.List (intercalate)


data CheckerState = MkCheckState { checkVars :: !(M.Map Variable T.Ty), checkTyVars :: ![TypeVar]}

initialCheckerState :: CheckerState 
initialCheckerState = MkCheckState M.empty []

data CheckerError =
  ErrNoAnnot !Variable
  | ErrUndefinedVar !Variable
  | ErrUndefinedTyVar !TypeVar
  | ErrTyCoForShift !T.Term !T.Ty
  | ErrKindNeq !T.Ty !T.Ty
  | ErrTypeNeq !T.Ty !T.Ty
  | ErrNotTyDecl !TypeName !T.Ty
  | ErrTypeArity !TypeName
  | ErrXtorArity !XtorName
  | ErrBadPattern ![XtorName] ![XtorName]
  | ErrCutKind !T.Ty !T.Ty
  | ErrBadType !D.Term !T.Ty
  | ErrUnclearTypeCut !D.Term !D.Term
  | ErrEnv !String !Loc

instance Error CheckerError where 
  getMessage (ErrNoAnnot var) = "No annotation for " <> show var <> ", cannot type check."
  getMessage (ErrUndefinedVar var) = "Variable " <> show var <> " was not defined "
  getMessage (ErrUndefinedTyVar tyv) = "Type Variable " <> show tyv <> " was not defined"
  getMessage (ErrTyCoForShift t ty) = "Cannot use co-type of " <> show ty <> " for shift term " <> show t
  getMessage (ErrKindNeq ty1 ty2) = "Kinds of types " <> show ty1 <> " and " <> show ty2 <> " are not equal"
  getMessage (ErrTypeNeq ty1 ty2) = "Types " <> show ty1 <> " and " <> show ty2 <> " should be equal"
  getMessage (ErrNotTyDecl tyn ty) = "Type " <> show ty <> " should be " <> show tyn
  getMessage (ErrTypeArity tyn) = "Wrong number of arguments for type " <> show tyn
  getMessage (ErrXtorArity xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrBadPattern exPts shouldPts) = "Malformed case: found patterns for " <> intercalate ", " (show <$> exPts) <> ", expected " <> intercalate ", " (show <$> shouldPts)
  getMessage (ErrCutKind ty1 ty2) = "Kind of types " <> show ty1 <> " and " <> show ty2 <> " in cut are not equal"
  getMessage (ErrBadType t ty) = "Cannot typecheck " <> show t <> " with type " <> show ty
  getMessage (ErrUnclearTypeCut t1 t2) = "Types of terms " <> show t1 <> " and " <> show t2 <> " in cut unclear"
  getMessage (ErrEnv str _) = str

  getLoc _ = defaultLoc
  toError = ErrEnv

newtype CheckM a = CheckM { getCheckM :: ReaderT Environment (StateT CheckerState (Except CheckerError)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Environment, MonadError CheckerError, MonadState CheckerState)

runCheckM :: Environment -> CheckM a -> Either CheckerError a
runCheckM env m = case runExcept (runStateT (runReaderT (getCheckM m) env) initialCheckerState) of 
  Left err -> Left err
  Right (x,_) -> Right x

addCheckerVar :: Variable -> T.Ty -> CheckM () 
addCheckerVar v ty = modify (\s -> MkCheckState (M.insert v ty (checkVars s)) (checkTyVars s))

addCheckerTyVar :: TypeVar -> CheckM ()
addCheckerTyVar tyv = modify (\s -> MkCheckState (checkVars s) (tyv:checkTyVars s))

getCheckerVars :: CheckM (M.Map Variable T.Ty)
getCheckerVars = gets checkVars

getCheckerTyVars :: CheckM [TypeVar] 
getCheckerTyVars = gets checkTyVars

withCheckerVars :: M.Map Variable T.Ty -> CheckM a -> CheckM  a
withCheckerVars newVars fun = do
  currVars <- gets checkVars
  modify (MkCheckState newVars . checkTyVars) 
  res <- fun  
  modify (MkCheckState currVars . checkTyVars)
  return res
