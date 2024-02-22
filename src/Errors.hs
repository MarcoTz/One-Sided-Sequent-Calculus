module Errors where 

import Common 
import Syntax.Typed.Types
import Syntax.Typed.Terms

data Error = 
  ErrArity !XtorName
  | ErrNotMatched !XtorName
  | ErrNameExists !String
  | ErrDeclExists !TypeName
  | ErrDeclUndefined !TypeName
  | ErrXtorExists !XtorName
  | ErrVarUndefined !Variable
  | ErrKindMisMatch !Pol !Pol
  | ErrTyNeq !Ty !Ty
  | ErrXtorUndefined !XtorName
  | ErrPatMalformed ![Pattern]
  | ErrParser !String
