module Driver.Definition where 

import Typed.Program

import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad 

data DriverState = MkDriverState { drvDebug :: !Bool, drvEnv :: ![DataDecl] } 

newtype DriverM a = DriverM { getDriverM :: StateT DriverState (ExceptT String IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadError String, MonadIO)


runDriverM :: [DataDecl] -> DriverM a -> IO(Either String (a,DriverState))
runDriverM decls m = runExceptT $ runStateT (getDriverM m) (MkDriverState False decls)

runDriverMDb :: [DataDecl] -> DriverM a -> IO (Either String (a,DriverState))
runDriverMDb decls m = runExceptT $ runStateT (getDriverM m) (MkDriverState True decls)


liftErr :: Either String a -> DriverM a
liftErr (Left err) = do 
  debug err
  throwError err
liftErr (Right a) = return a 

debug :: String -> DriverM () 
debug st = do  
  db <- gets drvDebug
  Control.Monad.when db $ liftIO (putStrLn ("\t" <> st))
