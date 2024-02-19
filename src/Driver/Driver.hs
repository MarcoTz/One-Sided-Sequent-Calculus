module Driver.Driver where 

import Untyped.Syntax qualified as S
import Typed.Syntax qualified as T
import Typed.Program
import TypeInference.GenerateConstraints
import TypeInference.SolveConstraints
import Pretty () 

import Control.Monad.State
import Control.Monad.Except 
import Control.Monad
import Data.List (intercalate)

defaultDebug :: Bool
defaultDebug = True 

data DriverState = MkDriverState { drvDebug :: !Bool, drvEnv :: ![Decl] } 

newtype DriverM a = DriverM { getDriverM :: StateT DriverState (ExceptT String IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadError String, MonadIO)


runDriverM :: [Decl] -> DriverM a -> IO(Either String (a,DriverState))
runDriverM decls m = runExceptT $ runStateT (getDriverM m) (MkDriverState defaultDebug decls)

liftErr :: Either String a -> DriverM a
liftErr (Left err) = throwError err
liftErr (Right a) = return a 

debugLn :: String
debugLn = "==================================================="
debug :: String -> DriverM () 
debug st = do  
  db <- gets drvDebug
  Control.Monad.when db $ liftIO (print st)

inferCommand :: S.Command -> DriverM T.Command
inferCommand c = do 
  decls <- gets drvEnv
  debug (" Inferring " <> show c <> " with environment " <> show decls)
  debug debugLn 
  (c',ctrs) <- liftErr (runGenCmd decls c)
  debug (" Constraints " <> intercalate "\n" (show <$> ctrs))
  (_,varmap,kndmap) <- liftErr (runSolve ctrs)
  debug (" Substitutions " <> show varmap <> "\n" <> show kndmap)
  return c'

inferTerm :: S.Term -> DriverM T.Term
inferTerm t = do 
  decls <- gets drvEnv 
  (c',ctrs) <- liftErr (runGenT decls t)
  (_,_varmap,_kndmap) <- liftErr (runSolve ctrs)
  return c'

