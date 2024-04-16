module GenerateConstraints.Errors (
  GenerateError (..)
) where

import Common (Xtorname,Typename)
import Loc (Loc)
import Errors (class Error)
import Syntax.Desugared.Types (Ty)

import Prelude (show, (<>),(<$>))
import Data.List (List, intercalate)

data GenerateError =
  ErrXtorArity    Loc Xtorname 
  | ErrTyArity    Loc Typename
  | ErrKindNeq    Loc Ty Ty
  | ErrBadPattern Loc (List Xtorname)
  | ErrOther      Loc String

instance Error GenerateError where 
  getMessage (ErrXtorArity _ xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrKindNeq _ ty1 ty2) = "Kinds of types " <> show ty1 <> " and " <> show ty2 <> " are not equal"
  getMessage (ErrBadPattern _ xts) = "Malformed pattern: " <> intercalate ", " (show <$> xts)
  getMessage (ErrTyArity _ tyn) = "Wrong number of arguments for type " <> show tyn 
  getMessage (ErrOther _ str) = str

  getLocation (ErrXtorArity loc _) = loc 
  getLocation (ErrTyArity loc _) = loc
  getLocation (ErrKindNeq loc _ _) = loc 
  getLocation (ErrBadPattern loc _) = loc 
  getLocation (ErrOther loc _) = loc

  toError = ErrOther
