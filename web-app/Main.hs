module Main where

import Driver.Definition
import Driver.Driver 
import Environment
import Eval.Definition
import Errors
import StringFormat

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
    Right (Left c,st) -> setSuccess (replStr $ show c) "" (envToStr $ drvEnv st)
    Right (Right (MkTrace c tr),st) -> setSuccess (replStr $ show c) (traceToStr tr) (envToStr $ drvEnv st)
  return ()


main :: IO () 
main = do 
  cp <- createCompiler
  setCompiler cp 
