module Main where

import Driver.Definition
import Driver.Driver 
import Environment
import Eval.Definition
import Errors
import Syntax.Typed.Terms
import Debug.Trace 
import Callback
import JSBits
import GHC.JS.Prim

foreign import javascript "((c => { globalCompiler = c}))"
  setCompiler :: Callback (JSVal -> IO ()) -> IO ()

createCompiler :: IO (Callback (JSVal -> IO ()))
createCompiler = syncCallback1 ThrowWouldBlock runProg

runProg :: JSVal -> IO () 
runProg val = do 
  let progSource = fromJSString val
  let drvSt = MkDriverState False emptyEnv
  res <- runDriverM drvSt (runStr progSource True)
  case res of 
    Left err  -> let msg = getMessage err in setError msg
    --(Either T.Command EvalTrace)
    Right (Left c,_) -> setSuccess (show c)
    Right (Right (MkTrace c tr),_) -> setSuccess (show c <> "<br/>Trace:<br/>"  <> traceToStr tr) 
  return ()

traceToStr :: [Command] -> String
traceToStr [] = ""
traceToStr (c:cs) = replStr (show c) <> "<br/>" <> traceToStr cs

repl :: Char -> String
repl '<' = "&lt;"
repl '+' = "&plus;"
repl '|' = "&vert;"
repl c = trace ("not replacing " <> [c]) $ [c]

replStr :: String -> String 
replStr = concatMap repl

main :: IO () 
main = do 
  cp <- createCompiler
  setCompiler cp 
