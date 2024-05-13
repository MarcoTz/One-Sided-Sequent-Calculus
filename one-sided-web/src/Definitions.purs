module Definitions (
  Input(..),
  RunResult(..),
  initialState,
  State,
  runProg
) where 

import Prelude (show,($),(<$>),(<>))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..),snd)
import Data.List (List,intercalate)
import Data.Maybe (fromMaybe)
import Data.Map (lookup,toUnfoldable)

import Driver.Definition (DriverState(..),initialDriverState, runDriverM)
import Driver.Driver (runStr)
import Driver.Errors (DriverError)
import Common (Modulename(..),Variable)
import Errors (showInSrc)
import Syntax.Kinded.Types (Ty)
import Syntax.Kinded.Terms (getType)
import Syntax.Kinded.Program (Program(..),VarDecl(..),emptyProg)
import Eval.Definition (EvalTrace)
import StandardLib (libMap)

data Input = ProgramInput String | RunProg
data RunResult = 
  ResErr {errMsg :: String,  errDebug::String, errTypes::String} 
  | ResSucc {succCmd::String, succTrace::String, succDebug::String, succTypes::String}
type State = {progSrc::String, runRes::RunResult}

initialState :: Input -> State
initialState _ = {
  progSrc:intercalate "\n" (lookup (Modulename "Bool") libMap), 
  runRes:ResSucc {succCmd:"", succTrace:"", succDebug:"", succTypes:""}
  }

runProg :: String -> RunResult  
runProg progSource = toRunResult progSource $ runDriverM initialDriverState (runStr (Modulename "WebInput") progSource)

toRunResult :: String -> Tuple (Either DriverError (Tuple Program EvalTrace)) DriverState -> RunResult
toRunResult src (Tuple (Left err) st) = 
  let {debugTr:db,typesTr:tys} = stateOutput st (emptyProg (Modulename "") "") in
  ResErr {errMsg:showInSrc err src, errDebug:db, errTypes:tys}
toRunResult _ (Tuple (Right (Tuple p@(Program prog) tr)) st) =
  let {debugTr:db,typesTr:tys} = stateOutput st p in
  ResSucc {
    succCmd:fromMaybe "" (show <$> prog.progMain),
    succTrace:show tr, 
    succDebug:db, 
    succTypes:tys} 

stateOutput :: DriverState -> Program -> {debugTr :: String, typesTr :: String}
stateOutput (MkDriverState {drvDebug:db, drvEnv:_}) prog = {debugTr:intercalate "\n" db, typesTr:getEnvTrace prog}
  where 
    getEnvTrace :: Program -> String
    getEnvTrace (Program prog') = do
      let progVars :: List VarDecl
          progVars = snd <$> toUnfoldable prog'.progVars
      let varsTys :: List (Tuple Variable Ty)
          varsTys = (\(VarDecl var) -> Tuple var.varName (getType var.varBody)) <$> progVars
      let varsShown :: List String
          varsShown = (\(Tuple v ty) -> show v <> " :: " <> show ty) <$> varsTys
      intercalate "\n" varsShown
