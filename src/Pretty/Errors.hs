module Pretty.Errors where 

import Errors 
import Pretty.Types () 
import Pretty.Terms ()

instance Show Error where 
  show (ErrArity xt)            = "Wrong arity for xtor " <> xt
  show (ErrNotMatched xt)       = "Xtor " <> xt <> " not matched in case"
  show (ErrDeclExists n)        = "Declaration " <> n <> " already exists"
  show (ErrXtorExists xt)       = "Xtor " <> xt <> " already exists"
  show (ErrVarUndefined v)      = "Variable " <> v <> " was not defined"
  show (ErrKindMisMatch k1 k2)  = "Kinds " <> show k1 <> " and " <> show k2 <> " cannot be unified"
  show (ErrTyNeq ty1 ty2)       = "Types " <> show ty1 <> " and " <> show ty2 <> " cannot be unified" 
  show (ErrXtorUndefined xt)    = "Xtor " <> show xt <> " was not defined" 
  show (ErrPatMalformed pts)    = "Patterns " <> show pts <> " not well-formed"
  show (ErrParser s)            = "Parser Error: " <> s
  show (ErrNameExists nm)       = "Name " <> nm <> "already exists"
  show (ErrDeclUndefined tyn)   = "Type " <> tyn <> " was not defined"

