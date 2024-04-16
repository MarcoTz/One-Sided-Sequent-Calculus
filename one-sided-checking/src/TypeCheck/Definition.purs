module TypeCheck.Definition (
  runCheckM,
  CheckerState,
  CheckM,
  getCheckerVars,
  getCheckerTyVars,
  addCheckerVar,
  addCheckerTyVar,
  withCheckerVars,
  getMTypeVar,
  getTypeVar
) where 

import Loc (Loc)
import Common (Variable,Typevar)
import Environment (Environment,lookupMVar,lookupMRec)
import Syntax.Typed.Types (Ty) as T
import Syntax.Kinded.Types (embedType)
import Syntax.Kinded.Program (VarDecl(..),RecDecl(..))
import TypeCheck.Errors (CheckerError(..))

import Prelude (bind,pure)
import Data.Map (Map,empty,insert,lookup,union)
import Data.List (List(..))
import Data.Unit (Unit, unit)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT,runStateT,modify,gets)
import Control.Monad.Except (Except, runExcept, throwError)

data CheckerState = MkCheckState { checkVars :: (Map Variable T.Ty), checkTyVars :: List Typevar}

initialCheckerState :: CheckerState 
initialCheckerState = MkCheckState {checkVars:empty, checkTyVars:Nil}


type CheckM a = ReaderT Environment (StateT CheckerState (Except CheckerError)) a 

runCheckM :: forall a. Environment -> CheckM a -> Either CheckerError a
runCheckM env m = case runExcept (runStateT (runReaderT m env) initialCheckerState) of 
  Left err -> Left err
  Right (Tuple x _) -> Right x

addCheckerVar :: Variable -> T.Ty -> CheckM Unit
addCheckerVar v ty = do 
  _ <- modify (\(MkCheckState s) -> MkCheckState (s {checkVars=(insert v ty s.checkVars)}))
  pure unit

addCheckerTyVar :: Typevar -> CheckM Unit
addCheckerTyVar tyv = do 
  _ <- modify (\(MkCheckState s) -> MkCheckState (s {checkTyVars=Cons tyv s.checkTyVars}))
  pure unit

getCheckerVars :: CheckM (Map Variable T.Ty)
getCheckerVars = gets (\(MkCheckState s) -> s.checkVars)

getCheckerTyVars :: CheckM (List Typevar)
getCheckerTyVars = gets (\(MkCheckState s) -> s.checkTyVars)

withCheckerVars :: forall a.Map Variable T.Ty -> CheckM a -> CheckM  a
withCheckerVars newVars fun = do
  currVars <- getCheckerVars 
  _ <- modify (\(MkCheckState s) -> MkCheckState s{checkVars=union currVars newVars}) 
  res <- fun  
  _ <- modify (\(MkCheckState s) -> MkCheckState s{checkVars=currVars}) 
  pure res

getMTypeVar :: Variable -> CheckM (Maybe T.Ty)
getMTypeVar v = do
  vars <- getCheckerVars 
  mvar <- lookupMVar v
  mrec <- lookupMRec v
  case Tuple (lookup v vars) (Tuple mvar mrec) of 
    (Tuple Nothing (Tuple Nothing Nothing)) -> pure Nothing 
    (Tuple (Just ty) (Tuple _ _)) -> pure (Just ty)
    (Tuple _ (Tuple (Just (VarDecl vdecl)) _)) -> pure (Just (embedType vdecl.varTy))
    (Tuple _ (Tuple _ (Just (RecDecl rdecl)))) -> pure (Just (embedType rdecl.recTy))

getTypeVar :: Loc -> Variable -> CheckM T.Ty 
getTypeVar loc v = do 
  mty <- getMTypeVar v 
  case mty of 
    Nothing -> throwError (ErrUndefinedVar loc v)
    Just ty -> pure ty
