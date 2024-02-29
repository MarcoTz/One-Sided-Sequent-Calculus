module Desugar.Definition where 

import Errors 
import Common 
import Environment
import Syntax.Typed.Program qualified as T
import Syntax.Parsed.Program qualified as P

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader


newtype DesugarState = MkDesugarState { desCurrDecl :: Maybe P.DataDecl} 

initialDesugarState :: DesugarState 
initialDesugarState = MkDesugarState Nothing

newtype DesugarM a = DesugarM { getDesugarM :: ReaderT Environment (StateT DesugarState (Except Error)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DesugarState, MonadError Error, MonadReader Environment)

runDesugarM :: Environment -> DesugarM a -> Either Error a
runDesugarM env m = case runExcept (runStateT (runReaderT (getDesugarM m) env) initialDesugarState) of
  Left err -> Left err 
  Right (x,_) ->  Right x 

setCurrDecl :: P.DataDecl -> DesugarM () 
setCurrDecl decl = modify (\_ -> MkDesugarState (Just decl))

getCurrDecl :: Error -> DesugarM P.DataDecl
getCurrDecl err = do 
  curr <- gets desCurrDecl 
  case curr of 
    Nothing -> throwError err 
    Just decl -> return decl

getTynPol :: TypeName -> DesugarM Pol 
getTynPol tyn = do 
  decl <- lookupDecl tyn 
  return $ T.declPol decl
