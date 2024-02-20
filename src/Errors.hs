module Errors where 

import Common 
import Typed.Types
import Typed.Syntax

data Error = 
  ErrArity !XtorName
  | ErrNotMatched !XtorName
  | ErrDeclExists !TypeName
  | ErrXtorExists !XtorName
  | ErrVarUndefined !Variable
  | ErrKindMisMatch !Pol !Pol
  | ErrTyNeq !Ty !Ty
  | ErrXtorUndefined !XtorName
  | ErrPatMalformed ![Pattern]
  | ErrParser !String
