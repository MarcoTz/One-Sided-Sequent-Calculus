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

data GenerateError =
  ErrXtorArity !XtorName
  | ErrKindNeq !Ty !Ty
  | ErrBadPattern ![XtorName]

instance Error GenerateError where 
  getMessage (ErrXtorArity xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrKindNeq ty1 ty2) = "Kinds of types " <> show ty1 <> " and " <> show ty2 <> " are not equal"
  getMessage (ErrBadPattern xts) = "Malformed pattern: " <> intercalate ", " (show <$> xts)
  getLoc _ = defaultLoc 
  toError _ _ = error "undefined"
