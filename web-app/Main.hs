module Main where

import Driver.Definition
import Driver.Driver 
import Environment
import Pretty.Driver ()
import Pretty.Errors()
import Callback
import JSBits
import GHC.JS.Prim

foreign import javascript "((c => { globalCompiler = c})"
  setCompiler :: CallBack (JSVal -> IO ()) -> IO ()

srcContents ::  String 
srcContents = "module Fun\ndata Fun(a:+,b:-):- \n{ Ap(a,b)\n}\n\nid :: forall X. Fun(X,X) : +;\nid := case { Ap(x,a) => <x | + | a> };\n\nid2 :: Forall X. Fun(X,X) : +;\n\nid2 := id;"

runProg :: JSVal -> IO () 
runProg val = do 
  let progSource = fromJSString val
  let drvSt = MkDriverState False emptyEnv
  res <- runDriverM drvSt (runStr mn)

main :: IO () 
main = do
  setCompiler runProg  
