module Pretty.Eval where 

import Eval.Definition

import Data.List (intercalate) 
import Pretty.Kinded () 

instance Show EvalTrace where 
  show (MkTrace c []) = "Result " <> show c
  show (MkTrace c tr) = "Result " <> show c <> "\nTrace:\n" <> intercalate "\n" (show <$> tr)
