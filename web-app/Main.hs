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
import Data.Char(ord)
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
    Left err  -> let msg = getMessage err in setError msg
    Right mainRes -> setSuccess "successfully ran program more characters, this should not be a problem"
  return ()

main :: IO () 
main = do 
  cp <- createCompiler
  setCompiler cp 
