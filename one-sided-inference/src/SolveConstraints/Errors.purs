module SolveConstraints.Errors (
  SolverError (..)
) where 

import Loc (Loc,defaultLoc)
import Common (Typename, Kind)
import Errors (class Error)
import Syntax.Typed.Types (Ty)

import Prelude ((<>),show)

data SolverError =
  ErrTyArity Typename 
  | ErrTyNeq Ty Ty
  | ErrKindNeq Kind Kind
  | ErrTypeKindNeq Ty Ty
  | ErrOther Loc String

instance Error SolverError where 
  getMessage (ErrTyArity tyn) = "Wrong number of type arguments for type " <> show tyn
  getMessage (ErrTyNeq ty1 ty2) = "Types " <> show ty1 <> " and " <> show ty2 <> " are not equal"
  getMessage (ErrKindNeq knd1 knd2) = "Kinds " <> show knd1 <> " and " <> show knd2 <> " are not equal"
  getMessage (ErrTypeKindNeq ty1 ty2) = "Kinds of types " <> show ty1 <> " and " <> show ty2 <> " are not equal"
  getMessage (ErrOther _ msg) = msg
  
  getLocation (ErrOther loc _) = loc
  getLocation _ = defaultLoc
  toError = ErrOther 
