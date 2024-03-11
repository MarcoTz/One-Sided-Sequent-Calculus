module TypeCheck.Definition where 

import Errors 
import Environment
import Common
import Syntax.Typed.Types qualified as T 
import Control.Monad.Except 
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M

data CheckerState = MkCheckState { checkVars :: !(M.Map Variable T.Ty), checkTyVars :: ![TypeVar]}

initialCheckerState :: CheckerState 
initialCheckerState = MkCheckState M.empty []

newtype CheckM a = CheckM { getCheckM :: ReaderT Environment (StateT CheckerState (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Environment, MonadError Error, MonadState CheckerState)

runCheckM :: Environment -> CheckM a -> Either Error a
runCheckM env m = case runExcept (runStateT (runReaderT (getCheckM m) env) initialCheckerState) of 
  Left err -> Left err
  Right (x,_) -> Right x

addVarPol :: Variable -> T.Ty -> CheckM () 
addVarPol v ty = modify (\s -> MkCheckState (M.insert v ty (checkVars s)) (checkTyVars s))

addTyVar :: TypeVar -> CheckM ()
addTyVar tyv = modify (\s -> MkCheckState (checkVars s) (tyv:checkTyVars s))
