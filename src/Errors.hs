module Errors where 

import Common 
import Syntax.Typed.Types

data Error = 
  ErrArityXtor !XtorName
  | ErrArityTy !TypeName
  | ErrNotMatched !XtorName
  | ErrNameExists !String
  | ErrDeclExists !TypeName
  | ErrDeclUndefined !TypeName
  | ErrXtorExists !XtorName
  | ErrVarUndefined !Variable
  | ErrKindMisMatch !Pol !Pol
  | ErrTyNeq !Ty !Ty
  | ErrXtorUndefined !XtorName
  | ErrPatMalformed ![XtorName]
  | ErrParser !String
