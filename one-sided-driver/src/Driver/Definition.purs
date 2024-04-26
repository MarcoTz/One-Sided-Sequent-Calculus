module Driver.Definition (
  DriverM,
  runDriverM,
  liftErr,
  debug,
  addVarDecl,
  addDecl,
  initialDriverState,
  inEnv,
  getProg,
  DriverState (..)
) where 

import Common (Modulename)
import Errors (class Error, convertError)
import Driver.Errors (DriverError(..))
import Environment (Environment(..), emptyEnv, addDeclEnv,addVarEnv)
import Syntax.Kinded.Program (Program, DataDecl, VarDecl)

import Prelude (bind, pure,($)) 
import Data.Tuple (Tuple)
import Data.Maybe (Maybe(..),isJust)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Unit (Unit,unit)
import Data.List (List(..),snoc)
import Control.Monad.State (State, modify,runState,gets)
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

inEnv :: Modulename -> DriverM Boolean
inEnv mn = do 
   (Environment env) <- gets (\(MkDriverState st) -> st.drvEnv)
   pure $ isJust (lookup mn env)

getProg :: Modulename -> DriverM Program
getProg mn = do
  (Environment env) <- gets (\(MkDriverState st) -> st.drvEnv)
  case lookup mn env of 
    Nothing -> throwError (ErrNotFound mn)
    Just p -> pure p
   
liftErr :: forall e a. Error e => Either e a -> Modulename -> String -> DriverM a
liftErr (Left err) mn wh = throwError (ErrWithWhere (convertError err) mn wh)
liftErr (Right a) _ _ = pure a 

debug :: String -> DriverM Unit
debug st = do
  _ <- modify (\(MkDriverState s) -> MkDriverState s{drvDebug = snoc s.drvDebug st })
  pure unit
