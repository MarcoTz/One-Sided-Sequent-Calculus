module Errors where 

import Common 
import Syntax.Typed.Types
import Syntax.Desugared.Types qualified as D

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
  | ErrUnexpectedKind !Kind
  | ErrTySchemeNeq !D.TypeScheme !D.TypeScheme
