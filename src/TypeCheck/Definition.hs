module TypeCheck.Definition where 

import Errors 
import Environment
import Common
import Syntax.Typed.Types

import Control.Monad.Except 
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M

data CheckerState = MkCheckState { checkVars :: !(M.Map Variable Ty), checkTyVars :: !(M.Map TypeVar Pol)}

initialCheckerState :: CheckerState 
initialCheckerState = MkCheckState M.empty M.empty

newtype CheckM a = CheckM { getCheckM :: ReaderT Environment (StateT CheckerState (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Environment, MonadError Error, MonadState CheckerState)

runCheckM :: Environment -> CheckM a -> Either Error a
runCheckM env m = case runExcept (runStateT (runReaderT (getCheckM m) env) initialCheckerState) of 
  Left err -> Left err
  Right (x,_) -> Right x

addVar :: Variable -> Ty -> CheckM () 
addVar v ty = modify (\s -> MkCheckState (M.insert v ty (checkVars s)) (checkTyVars s))

remVar :: Variable -> CheckM () 
remVar v = modify (\s -> MkCheckState (M.delete v (checkVars s)) (checkTyVars s))

addTyVar :: PolVar -> CheckM () 
addTyVar (MkPolVar tyv pol) = modify (\s -> MkCheckState (checkVars s) (M.insert tyv pol (checkTyVars s)))

remTyVar :: PolVar -> CheckM () 
remTyVar (MkPolVar tyv _) = modify (\s -> MkCheckState (checkVars s) (M.delete tyv (checkTyVars s)))
