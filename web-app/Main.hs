module Main where

import Driver.Definition
import Driver.Driver 
import Environment
import Pretty.Driver ()

srcContents ::  String 
srcContents = "module Fun\ndata Fun(a:+,b:-):- \n{ Ap(a,b)\n}\n\nid :: forall X. Fun(X,X) : +;\nid := case { Ap(x,a) => <x | + | a> };\n\nid2 :: Forall X. Fun(X,X) : +;\n\nid2 := id;"

main :: IO()
main = do
  let drvSt = MkDriverState True emptyEnv
  res <- runDriverM drvSt (runStr srcContents)
  print res
 
