module GenerateConstraints.Errors (
  GenerateError (..)
) where

import Common
import Errors
import Loc
import Syntax.Typed.Types
import Pretty.Common ()
import Pretty.Typed ()

import Data.List (intercalate)

data GenerateError where 
  ErrXtorArity  :: Loc -> XtorName -> GenerateError
  ErrTyArity    :: Loc -> TypeName -> GenerateError
  ErrKindNeq    :: Loc -> Ty -> Ty -> GenerateError
  ErrBadPattern :: Loc -> [XtorName] -> GenerateError
  ErrOther      :: Loc -> String -> GenerateError

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

