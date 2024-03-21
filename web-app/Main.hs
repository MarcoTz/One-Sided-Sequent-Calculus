module Main where

import Driver.Definition
import Driver.Driver 
import Environment
import Pretty.Driver ()
import Pretty.Errors()
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
  res <- runDriverM drvSt (runStr progSource)
  setEvaluation (show res)
  return ()

main :: IO () 
main = do 
  cp <- createCompiler
  setCompiler cp 
