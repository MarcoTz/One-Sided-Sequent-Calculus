module Main where 

import Effect (Effect)
import Effect.Console (logShow)

import Prelude (pure, show,(<>))
import Data.Unit (Unit,unit)
import Data.List (List (..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))

import Driver.Definition (DriverState(..), runDriverM)
import Driver.Driver (runStr)
import Errors (getMessage)
import Environment (emptyEnv)
import Eval.Definition (EvalTrace(..))

setError :: String -> Effect Unit 
setError str = logShow str 

setSuccess :: String -> String -> String -> Effect Unit 
setSuccess str1 str2 str3 = logShow (str1 <> "\n" <> str2 <> "\n" <> str3) 

runProg :: String -> Effect Unit
runProg progSource = do 
  let drvSt = MkDriverState { drvDebug:Nil,drvEnv:emptyEnv}
  let res = runDriverM drvSt (runStr progSource true)
  case res of 
      Left err -> setError (getMessage err)
      Right (Tuple (Left c) st) -> setSuccess (show c) "" (show st)
      Right (Tuple (Right (MkTrace c tr)) st) -> setSuccess (show c) (show tr) (show st)


main :: Effect Unit 
main = pure unit
