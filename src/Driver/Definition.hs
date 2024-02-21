module Driver.Definition where 

import Syntax.Typed.Program
import Errors
import Pretty.Errors  ()

import Control.Monad.State 
import Control.Monad.Except 
import Control.Monad 

data DriverState = MkDriverState { drvDebug :: !Bool, drvEnv :: !Program } 

newtype DriverM a = DriverM { getDriverM :: StateT DriverState (ExceptT Error IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadError Error, MonadIO)

initialDriverState :: DriverState
initialDriverState = MkDriverState { drvDebug = False, drvEnv = emptyProg} 

runDriverM :: DriverState -> DriverM a -> IO(Either Error (a,DriverState))
runDriverM drvst m = runExceptT $ runStateT (getDriverM m) drvst 

addDecl :: DataDecl -> DriverM ()
addDecl decl = modify (\s -> MkDriverState (drvDebug s) (addDeclToProgram decl (drvEnv s)))

addVar :: VarDecl -> DriverM ()
addVar var = modify (\s -> MkDriverState (drvDebug s) (addVarToProgram var (drvEnv s)))

liftErr :: Either Error a -> DriverM a
liftErr (Left err) = do 
  debug (show err)
  throwError err
liftErr (Right a) = return a 

debug :: String -> DriverM () 
debug st = do  
  db <- gets drvDebug
  Control.Monad.when db $ liftIO (putStrLn st)
