module Driver.Definition (
  DriverM,
  runDriverM,
  liftErr,
  debug,
  setDebug,
  addRecDecl,
  addVarDecl,
  addDecl,
  DriverState (..),
  DriverError (..)
) where 

import Driver.Errors
import Syntax.Typed.Program
import Errors
import Common
import Environment

import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad 

data DriverState = MkDriverState { drvDebug :: !Bool, drvEnv :: !Environment} 

newtype DriverM a = DriverM { getDriverM :: StateT DriverState (ExceptT DriverError IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadError DriverError, MonadIO)

runDriverM :: DriverState -> DriverM a -> IO (Either DriverError (a,DriverState))
runDriverM drvst m = runExceptT (runStateT (getDriverM m) drvst)

addDecl :: Modulename -> DataDecl -> DriverM ()
addDecl nm decl = modify (\s -> MkDriverState (drvDebug s) (addDeclEnv nm decl (drvEnv s)) )

addVarDecl :: Modulename -> VarDecl -> DriverM ()
addVarDecl nm var = modify (\s -> MkDriverState (drvDebug s) (addVarEnv nm var (drvEnv s)))

addRecDecl :: Modulename -> RecDecl -> DriverM () 
addRecDecl nm rec = modify (\s -> MkDriverState (drvDebug s) (addRecEnv nm rec (drvEnv s)))

setDebug :: Bool -> DriverM () 
setDebug b = modify (MkDriverState b . drvEnv)

liftErr :: Error e => Either e a -> String -> DriverM a
liftErr (Left err) wh = throwError (ErrWithWhere (convertError err) wh)
liftErr (Right a) _ = return a 

debug :: String -> DriverM () 
debug st = do  
  db <- gets drvDebug
  Control.Monad.when db $ liftIO (putStrLn st)
