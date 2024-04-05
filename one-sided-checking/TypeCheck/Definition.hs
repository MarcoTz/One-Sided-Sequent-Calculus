module TypeCheck.Definition (
  runCheckM,
  CheckM,
  getCheckerVars,
  getCheckerTyVars,
  addCheckerVar,
  addCheckerTyVar,
  withCheckerVars,
  getMTypeVar,
  CheckerError (..)
) where 

import Environment
import Common
import TypeCheck.Errors
import Syntax.Typed.Types   qualified as T 
import Syntax.Typed.Program qualified as T
import Pretty.Typed () 
import Pretty.Desugared ()

import Control.Monad.Except 
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M


data CheckerState = MkCheckState { checkVars :: !(M.Map Variable T.Ty), checkTyVars :: ![TypeVar]}

initialCheckerState :: CheckerState 
initialCheckerState = MkCheckState M.empty []


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

getMTypeVar :: Variable -> CheckM (Maybe T.Ty)
getMTypeVar v = do
  vars <- getCheckerVars 
  mvar <- lookupMVar v
  case (M.lookup v vars,mvar) of 
    (Nothing,Nothing) -> return Nothing 
    (Just ty,_) -> return (Just ty)
    (_,Just ty) -> return (Just $ T.varTy ty)
