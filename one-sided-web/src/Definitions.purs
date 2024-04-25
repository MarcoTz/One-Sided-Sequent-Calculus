module Definitions (
  Input(..),
  RunResult(..),
  initialState,
  State,
  runProg
) where 

import Prelude (show,($))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.List (intercalate)
import Data.Map (lookup)

import Driver.Definition (DriverState(..),initialDriverState, runDriverM)
import Driver.Driver (runStr)
import Driver.Errors (DriverError)
import Common (Modulename(..))
import Errors (showInSrc)
import Syntax.Kinded.Terms (Command)
import Eval.Definition (EvalTrace(..))
import StandardLib (libMap)

data Input = ProgramInput String | RunProg
data RunResult = 
  ResErr {errMsg :: String,  errDebug::String} 
  | ResSucc {succCmd::String, succTrace::String, succDebug::String}
type State = {progSrc::String, runRes::RunResult}

initialState :: Input -> State
initialState _ = {
  progSrc:intercalate "\n" (lookup (Modulename "Stream") libMap), 
  runRes:ResSucc {succCmd:"", succTrace:"", succDebug:""}
  }

runProg :: String -> RunResult  
runProg progSource = toRunResult progSource $ runDriverM initialDriverState (runStr progSource true)

toRunResult :: String -> Tuple (Either DriverError (Either Command EvalTrace)) DriverState -> RunResult
toRunResult src (Tuple (Left err) st) = 
  ResErr {errMsg:showInSrc err src, errDebug:stateOutput st}
toRunResult _ (Tuple (Right (Left c)) st) =
  ResSucc {succCmd:show c,succTrace:"", succDebug:stateOutput st} 
toRunResult _ (Tuple (Right (Right (MkTrace c tr))) st) = 
  ResSucc {succCmd:show c, succTrace:show tr, succDebug:stateOutput st}

stateOutput :: DriverState -> String 
stateOutput (MkDriverState {drvDebug:db, drvEnv:_env}) = intercalate "\n" db
