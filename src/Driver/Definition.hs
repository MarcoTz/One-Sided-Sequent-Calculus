module Driver.Definition where 

import Syntax.Typed.Program
import Errors
import Common
import Pretty.Errors  ()
import Environment

import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad 

data DriverState = MkDriverState { drvDebug :: !Bool, drvEnv :: !Environment, drvLoaded :: ![Modulename] } 

newtype DriverM a = DriverM { getDriverM :: StateT DriverState (ExceptT Error IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadError Error, MonadIO)

initialDriverState :: DriverState
initialDriverState = MkDriverState False emptyEnv []

runDriverM :: DriverState -> DriverM a -> IO(Either Error (a,DriverState))
runDriverM drvst m = runExceptT $ runStateT (getDriverM m) drvst 

addDecl :: Modulename -> DataDecl -> DriverM ()
addDecl nm decl = modify (\s -> MkDriverState (drvDebug s) (addDeclEnv nm decl (drvEnv s)) (drvLoaded s) )

addVarDecl :: Modulename -> VarDecl -> DriverM ()
addVarDecl nm var = modify (\s -> MkDriverState (drvDebug s) (addVarEnv nm var (drvEnv s)) (drvLoaded s))

addLoaded :: Modulename -> DriverM ()
addLoaded mn = modify (\s -> MkDriverState (drvDebug s) (drvEnv s) (mn : drvLoaded s))

liftErr :: Either Error a -> DriverM a
liftErr (Left err) = throwError err
liftErr (Right a) = return a 

debug :: String -> DriverM () 
debug st = do  
  db <- gets drvDebug
  Control.Monad.when db $ liftIO (putStrLn st)
