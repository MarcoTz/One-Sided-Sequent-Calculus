module Driver.Driver where 

import Untyped.Syntax qualified as S
import Typed.Syntax qualified as T
import Typed.Program
import TypeInference.GenerateConstraints
import TypeInference.SolveConstraints

import Control.Monad.State
import Control.Monad.Except 

data DriverState = MkDriverState { drvDebug :: !Bool, drvEnv :: ![Decl] } 

newtype DriverM a = DriverM { getDriverM :: StateT DriverState (Except String) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DriverState, MonadError String)


runDriverM :: [Decl] -> DriverM a -> Either String () 
runDriverM decls m = case runExcept (runStateT (getDriverM m) (MkDriverState False decls)) of
  Left err -> Left err 
  Right (_, _) ->  Right () 

liftErr :: Either String a -> DriverM a
liftErr (Left err) = throwError err
liftErr (Right a) = return a 

inferCommand :: S.Command -> DriverM T.Command
inferCommand c = do 
  decls <- gets drvEnv
  (c',ctrs) <- liftErr (runGenCmd decls c)
  (_,_varmap,_kndmap) <- liftErr (runSolve ctrs)
  return c'

inferTerm :: S.Term -> DriverM T.Term
inferTerm t = do 
  decls <- gets drvEnv 
  (c',ctrs) <- liftErr (runGenT decls t)
  (_,_varmap,_kndmap) <- liftErr (runSolve ctrs)
  return c'

