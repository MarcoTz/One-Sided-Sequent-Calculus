module SolveConstraints.Errors (
  SolverError (..)
) where 

import Common
import Errors
import Loc
import Syntax.Typed.Types
import Pretty.Common ()
import Pretty.Typed ()

data SolverError = 
  ErrTyArity !TypeName 
  | ErrTyNeq !Ty !Ty
  | ErrKindNeq !Kind !Kind
  | ErrTypeKindNeq !Ty !Ty

instance Error SolverError where 
  getMessage (ErrTyArity tyn) = "Wrong number of type arguments for type " <> show tyn
  getMessage (ErrTyNeq ty1 ty2) = "Types " <> show ty1 <> " and " <> show ty2 <> " are not equal"
  getMessage (ErrKindNeq knd1 knd2) = "Kinds " <> show knd1 <> " and " <> show knd2 <> " are not equal"
  getMessage (ErrTypeKindNeq ty1 ty2) = "Kinds of types " <> show ty1 <> " and " <> show ty2 <> " are not equal"
  getLocation _ = defaultLoc
  toError _ = error "not implemented"
