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
  let errStr err = "Error inferring program :\n <div class=\"resError\">" <> show err <> "</div>"
  let succStr res' = "Program was successfully inferred and ran with result: \n <div class=\"resSucc\">" <> show res' <> "</div>"
  case res of 
    Left err  -> setEvaluationResult (errStr err)
    Right (Nothing,_) -> setEvaluationResult (succStr "No function main was defined")
    Right (Just mainRes,_) -> setEvaluationResult (succStr mainRes)
  return ()

main :: IO () 
main = do 
  cp <- createCompiler
  setCompiler cp 
