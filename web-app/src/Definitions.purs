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

import Prelude (class Show,(<>), show,($))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

import Driver.Definition (DriverState,initialDriverState, runDriverM)
import Driver.Driver (runStr)
import Driver.Errors (DriverError)
import Errors (getMessage)
import Syntax.Kinded.Terms (Command)
import Eval.Definition (EvalTrace(..))

data Input = ExampleSelect Examples | ProgramInput String | RunProg


data RunResult = ResErr String | ResSucc String String String
instance Show RunResult where 
  show (ResErr str) = str 
  show (ResSucc str1 str2 str3) = str1 <> str2 <> str3
type State = {progSrc::String, runRes::RunResult}

initialState :: Input -> State
initialState (ExampleSelect ex) = {progSrc:getExSource ex,runRes:ResSucc "" "" ""} 
initialState _ = {progSrc:tupleStr, runRes:ResSucc "" "" ""}

runProg :: String -> RunResult  
runProg progSource = toRunResult $ runDriverM initialDriverState (runStr progSource true)

toRunResult :: (Either DriverError (Tuple (Either Command EvalTrace) DriverState)) -> RunResult
toRunResult (Left err) = ResErr (getMessage err)
toRunResult (Right (Tuple (Left c) st)) = ResSucc (show c) "" (show st)
toRunResult (Right (Tuple (Right (MkTrace c tr)) st)) = ResSucc (show c) (show tr) (show st)


data Examples = ExTuple | ExList | ExFun | ExNat

indexToEx :: Int -> Examples 
indexToEx 0 = ExTuple
indexToEx 1 = ExList 
indexToEx 2 = ExNat
indexToEx 3 = ExFun 
indexToEx _ = ExTuple

getExSource :: Examples -> String 
getExSource ExTuple = tupleStr
getExSource ExList = listStr
getExSource ExFun = funStr
getExSource ExNat = natStr

tupleStr :: String 
tupleStr = "data Pair(a:+,b:+){\n\tTup(a,b)\n}\n\ndata Nat {\n\tZ,\n\tS(Nat)\n}\n\ndata Fun(a:+,b:-) {\n\tAp(a,b)\n}\n\nprintCons :: forall X.X;\nprintCons := mu x.Print x;\n\nswap :: forall X Y. Fun(Pair(X,Y),Pair(Y,X));\nswap := case { Ap(p,a) =&gt;\n\t&lt; case { \n\t\tTup(b,c) =&gt; &lt; Tup(c,b) | Pair(Y,X): CBV | a&gt;\n\t} | CBV | p&gt;\n};\n\npair1 :: Pair(Nat,Nat);\npair1 := Tup(Z,S(Z));\n\n main := &lt; swap | CBV | Ap(pair1,printCons)&gt;;"

listStr :: String 
listStr = ""

natStr :: String 
natStr = "data Nat {\n\tZ,\n\tS(Nat)\n}\n\ndata Fun(a:+,b:-) { \n\tAp(a,b)\n}\n\nprintCons :: Forall X.X;\nprintCons := mu x. Print x;\n\npred :: Fun(Nat,Nat);\npred := case { Ap(n,a) =&gt;\n\t&lt; case { \n\t\tZ =&gt; &lt; Z | CBV | a&gt;,\nt\t\tS(m) =&gt; &lt; m | CBV | a &gt;\n\t} | CBV | n&gt;\n};\n\nnat1 :: Nat;\nnat1 := S(S(Z));\n\n main := &lt; pred | CBV | Ap(nat1,printCons)&gt;;"

funStr :: String 
funStr = "data Fun(a:+,b:-){ \n\tAp(a,b)\n}\n\ndata Unit {MkUnit}\n\nprintCons :: Forall X.X;\nprintCons := mu x. Print x;\n\nid :: forall X. Fun(X,X);\nid := case { Ap(x,a) =&gt; &lt;x | CBV | a&gt; };\n\nmain := &lt; id | CBV | Ap(MkUnit,printCons)&gt;; "
