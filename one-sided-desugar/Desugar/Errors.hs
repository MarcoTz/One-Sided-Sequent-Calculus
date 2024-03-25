module Desugar.Errors (DesugarError (..)) where 

import Common 
import Loc
import Errors
import Syntax.Desugared.Types 
import Pretty.Common ()
import Pretty.Desugared ()

data DesugarError = 
  ErrVariable !Variable
  | ErrMultiple !String
  | ErrMultipleAnnot !Variable !PolTy !PolTy
  | EnvErr !String !Loc

instance Error DesugarError where 
  getMessage (EnvErr msg _) = msg 
  getMessage (ErrVariable var) = "Definition for variable " <> show var <> "could not be found"
  getMessage (ErrMultiple str) = str <> " was defined multiple times"
  getMessage (ErrMultipleAnnot var ty1 ty2) = "Multiple incompatible annotations for variable" <> show var <> ": " <> show ty1 <> " and " <> show ty2
  getLoc _ = defaultLoc
  toError = EnvErr
