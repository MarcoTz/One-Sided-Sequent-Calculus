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
import Data.List (List,intercalate,concatMap)
import Data.Map (lookup,toUnfoldable)

import Driver.Definition (DriverState(..),initialDriverState, runDriverM)
import Driver.Driver (runStr)
import Driver.Errors (DriverError)
import Common (Modulename(..),Variable)
import Environment (Environment(..))
import Errors (showInSrc)
import Syntax.Kinded.Terms (Command)
import Syntax.Kinded.Types (Ty)
import Syntax.Kinded.Program (Program(..),VarDecl(..))
import Eval.Definition (EvalTrace(..))
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
runProg progSource = toRunResult progSource $ runDriverM initialDriverState (runStr (Modulename "WebInput") progSource true)

toRunResult :: String -> Tuple (Either DriverError (Either Command EvalTrace)) DriverState -> RunResult
toRunResult src (Tuple (Left err) st) = 
  let {debugTr:db,typesTr:tys} = stateOutput st in
  ResErr {errMsg:showInSrc err src, errDebug:db, errTypes:tys}
toRunResult _ (Tuple (Right (Left c)) st) =
  let {debugTr:db,typesTr:tys} = stateOutput st in
  ResSucc {succCmd:show c,succTrace:"", succDebug:db, succTypes:tys} 
toRunResult _ (Tuple (Right (Right (MkTrace c tr))) st) = 
  let {debugTr:db,typesTr:tys}  =stateOutput st in
  ResSucc {succCmd:show c, succTrace:show tr, succDebug:db, succTypes:tys}

stateOutput :: DriverState -> {debugTr :: String, typesTr :: String}
stateOutput (MkDriverState {drvDebug:db, drvEnv:env}) = {debugTr:intercalate "\n" db, typesTr:getEnvTrace env}
  where 
    getEnvTrace :: Environment -> String
    getEnvTrace (Environment env') = do
      let progList :: List Program
          progList = snd <$> (toUnfoldable env')
      let progVars :: List VarDecl
          progVars = concatMap (\(Program prog) -> snd <$> toUnfoldable prog.progVars) progList
      let varsTys :: List (Tuple Variable Ty)
          varsTys = (\(VarDecl var) -> Tuple var.varName var.varTy) <$> progVars
      let varsShown :: List String
          varsShown = (\(Tuple v ty) -> show v <> " :: " <> show ty) <$> varsTys
      intercalate "\n" varsShown
