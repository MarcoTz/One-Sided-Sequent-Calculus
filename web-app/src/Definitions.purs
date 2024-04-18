module Definitions (
  Input(..),
  Examples (..),
  RunResult(..),
  initialState,
  State,
  getExSource,
  runProg,
  indexToEx
) where 

import Prelude (show,($))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.List (intercalate)

import Driver.Definition (DriverState(..),initialDriverState, runDriverM)
import Driver.Driver (runStr)
import Driver.Errors (DriverError)
import Errors (showInSrc)
import Syntax.Kinded.Terms (Command)
import Eval.Definition (EvalTrace(..))

data Input = ExampleSelect Examples | ProgramInput String | RunProg
data RunResult = ResErr String String | ResSucc String String String
type State = {progSrc::String, runRes::RunResult}

initialState :: Input -> State
initialState (ExampleSelect ex) = {progSrc:intercalate "\n" (getExSource ex),runRes:ResSucc "" "" ""} 
initialState _ = {progSrc:intercalate "\n" tupleStr, runRes:ResSucc "" "" ""}

runProg :: String -> RunResult  
runProg progSource = toRunResult progSource $ runDriverM initialDriverState (runStr progSource true)

toRunResult :: String -> Tuple (Either DriverError (Either Command EvalTrace)) DriverState -> RunResult
toRunResult src (Tuple (Left err) st) = ResErr (showInSrc err src) (stateOutput st)
toRunResult _ (Tuple (Right (Left c)) st) = ResSucc (show c) (stateOutput st) ""
toRunResult _ (Tuple (Right (Right (MkTrace c tr))) st) = ResSucc (show c) (stateOutput st) (show tr)

stateOutput :: DriverState -> String 
stateOutput (MkDriverState {drvDebug:db, drvEnv:_env}) = intercalate "\n" db


data Examples = ExTuple | ExList | ExFun | ExNat

indexToEx :: Int -> Examples 
indexToEx 0 = ExTuple
indexToEx 1 = ExList 
indexToEx 2 = ExNat
indexToEx 3 = ExFun 
indexToEx _ = ExTuple

getExSource :: Examples -> Array String 
getExSource ExTuple = tupleStr
getExSource ExList = listStr
getExSource ExFun = funStr
getExSource ExNat = natStr

tupleStr :: Array String 
tupleStr = [
  "module Pair",
  "data Pair(a:+,b:+){" ,
  "\tTup(a,b)",
   "}",
  "",
  "",
  "data Nat {",
  "\tZ,",
  "\tS(Nat)",
  "}",
  "" ,
  "",
  "data Fun(a:+,b:-) {",
  "\tAp(a,b)",
  "}",
  "",
  "printCons :: forall X.X;",
  "printCons := mu x.Print x;",
  "",
  "swap :: forall X Y. Fun(Pair(X,Y),Pair(Y,X));",
  "swap := case { Ap(p,a) =>",
  "\t< case { ",
  "\t\tTup(b,c) => < Tup(c,b) | Pair(Y,X): CBV | a>",
  "\t} | CBV | p>",
  "};",
  "",
  "pair1 :: Pair(Nat,Nat);",
  "pair1 := Tup(Z,S(Z));",
  "",
  " main := < swap | CBV | Ap(pair1,printCons)>;"]

listStr :: Array String 
listStr = ["module List" ,
  "data List(a:+){",
  "\tNil,",
  "\tCons(a,List(a))",
  "}"]

natStr :: Array String 
natStr = ["module Nat",
  "data Nat {",
  "\tZ,",
  "\tS(Nat)",
  "}",
  "",
  "data Fun(a:+,b:-) { ",
  "\tAp(a,b)",
  "}",
  "",
  "printCons :: Forall X.X;",
  "printCons := mu x. Print x;",
  "",
  "pred :: Fun(Nat,Nat);",
  "pred := case { Ap(n,a) =>",
  "\t< case { ",
  "\t\tZ => < Z | CBV | a>,",
  "\t\tS(m) => < m | CBV | a >",
  "\t} | CBV | n>",
  "};",
  "",
  "nat1 :: Nat;",
  "nat1 := S(S(Z));",
  "",
  "main := < pred | CBV | Ap(nat1,printCons)>;"]

funStr :: Array String 
funStr = ["module Fun",
  "data Fun(a:+,b:-){ ",
  "\tAp(a,b)",
  "}",
  "",
  "data Unit {MkUnit}",
  "",
  "printCons :: Forall X.X;",
  "printCons := mu x. Print x;",
  "",
  "id :: forall X. Fun(X,X);",
  "id := case { Ap(x,a) => <x | CBV | a> };",
  "",
  "main := < id | CBV | Ap(MkUnit,printCons)>; "]
