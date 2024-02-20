module Driver.Definition where 

import Typed.Program

import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad 

defaultDebug :: Bool
defaultDebug = True

data DriverState = MkDriverState { drvDebug :: !Bool, drvEnv :: ![DataDecl] } 

newtype DriverM a = DriverM { getDriverM :: StateT DriverState (ExceptT String IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadError String, MonadIO)


runDriverM :: [DataDecl] -> DriverM a -> IO(Either String (a,DriverState))
runDriverM decls m = runExceptT $ runStateT (getDriverM m) (MkDriverState defaultDebug decls)

liftErr :: Either String a -> DriverM a
liftErr (Left err) = do 
  debug err
  throwError err
liftErr (Right a) = return a 

debugLn :: String
debugLn = "==================================================="
debug :: String -> DriverM () 
debug st = do  
  db <- gets drvDebug
  Control.Monad.when db $ liftIO (print st)
