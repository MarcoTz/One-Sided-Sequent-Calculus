module Main where

import Driver.Definition
import Driver.Driver 
import Environment
import Errors
import Callback
import JSBits
import GHC.JS.Prim
import Pretty.Eval ()
import Pretty.Driver ()

foreign import javascript "((c => { globalCompiler = c}))"
  setCompiler :: Callback (JSVal -> IO ()) -> IO ()

createCompiler :: IO (Callback (JSVal -> IO ()))
createCompiler = syncCallback1 ThrowWouldBlock runProg

runProg :: JSVal -> IO () 
runProg val = do 
  let progSource = fromJSString val
  let drvSt = MkDriverState False emptyEnv
  res <- runDriverM drvSt (runStr progSource False)
  case res of 
    Left err  -> setError (getMessage err)
    Right mainRes -> setSuccess "successfully ran program"
  return ()

main :: IO () 
main = do 
  cp <- createCompiler
  setCompiler cp 
