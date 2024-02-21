module Desugar.Definition where 

import Errors 
import Syntax.Desugared.Program

import Control.Monad.State
import Control.Monad.Except


data DesugarState = MkDesugarState { desDecls :: ![DataDecl] } 

initialDesugarState :: DesugarState 
initialDesugarState = MkDesugarState [] 

newtype DesugarM a = DesugarM { getDesugarM :: StateT DesugarState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DesugarState, MonadError Error)

runDesugarM :: DesugarM a -> Either Error a
runDesugarM m = case runExcept (runStateT (getDesugarM m) initialDesugarState) of
  Left err -> Left err 
  Right (x,_) ->  Right x 

addDataDecl :: DataDecl -> DesugarM () 
addDataDecl decl = modify (\s -> MkDesugarState (decl : desDecls s))
