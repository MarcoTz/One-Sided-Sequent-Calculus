module TypeCheck.Definition where 

import Errors 
import Environment
import Common
import Syntax.Typed.Types

import Control.Monad.Except 
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M

newtype CheckerState = MkCheckState { checkVars :: M.Map Variable Ty }

initialCheckerState :: CheckerState 
initialCheckerState = MkCheckState M.empty 

newtype CheckM a = CheckM { getCheckM :: ReaderT Environment (StateT CheckerState (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Environment, MonadError Error, MonadState CheckerState)

runGenM :: Environment -> CheckM a -> Either Error a
runGenM env m = case runExcept (runStateT (runReaderT (getCheckM m) env) initialCheckerState) of 
  Left err -> Left err
  Right (x,_) -> Right x

addVar :: Variable -> Ty -> CheckM () 
addVar v ty = modify (\s -> MkCheckState (M.insert v ty (checkVars s)))

