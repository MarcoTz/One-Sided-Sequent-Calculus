module Desugar.Definition where 


import Control.Monad.State
import Control.Monad.Except

import Errors 

data DesugarState = MkDesugarState { placeholder :: !Bool, placeHolder2 :: !Bool } 

initialDesugarState :: DesugarState 
initialDesugarState = MkDesugarState True True

newtype DesugarM a = DesugarM { getDesugarM :: StateT DesugarState (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadState DesugarState, MonadError Error)

runDesugarM :: DesugarM a -> Either Error a
runDesugarM m = case runExcept (runStateT (getDesugarM m) initialDesugarState) of
  Left err -> Left err 
  Right (x,_) ->  Right x 
