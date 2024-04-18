module Driver.Definition (
  DriverM,
  runDriverM,
  liftErr,
  debug,
  addRecDecl,
  addVarDecl,
  addDecl,
  initialDriverState,
  DriverState (..)
) where 

import Common (Modulename)
import Errors (class Error, convertError)
import Driver.Errors (DriverError(..))
import Environment (Environment, emptyEnv, addDeclEnv,addVarEnv,addRecEnv)
import Syntax.Kinded.Program (DataDecl, VarDecl, RecDecl)

import Prelude (bind, pure) 
import Data.Tuple (Tuple)
import Data.Either (Either(..))
import Data.Unit (Unit,unit)
import Data.List (List(..),snoc)
import Control.Monad.State (State, modify,runState)
import Control.Monad.Except (ExceptT, throwError, runExceptT)


data DriverState = MkDriverState { drvEnv :: Environment, drvDebug :: List String} 

initialDriverState :: DriverState
initialDriverState = MkDriverState {drvEnv:emptyEnv, drvDebug:Nil}

type DriverM a = ExceptT DriverError (State DriverState) a 

runDriverM :: forall a.DriverState -> DriverM a -> Tuple (Either DriverError a) DriverState
runDriverM drvst m = runState (runExceptT m) drvst

addDecl :: Modulename -> DataDecl -> DriverM Unit
addDecl nm decl = do 
  _ <- modify (\(MkDriverState s) -> MkDriverState s{drvEnv=addDeclEnv nm decl s.drvEnv}) 
  pure unit

addVarDecl :: Modulename -> VarDecl -> DriverM Unit
addVarDecl nm var = do 
  _ <- modify (\(MkDriverState s) -> MkDriverState s{drvEnv=addVarEnv nm var s.drvEnv})
  pure unit

addRecDecl :: Modulename -> RecDecl -> DriverM Unit
addRecDecl nm rec = do 
  _ <- modify (\(MkDriverState s) -> MkDriverState s{drvEnv=addRecEnv nm rec s.drvEnv})
  pure unit

liftErr :: forall e a. Error e => Either e a -> String -> DriverM a
liftErr (Left err) wh = throwError (ErrWithWhere (convertError err) wh)
liftErr (Right a) _ = pure a 

debug :: String -> DriverM Unit
debug st = do
  _ <- modify (\(MkDriverState s) -> MkDriverState s{drvDebug = snoc s.drvDebug st })
  pure unit
