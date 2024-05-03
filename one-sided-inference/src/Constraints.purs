module Constraints (
  ConstraintSet (..),
  Constr (..),
  showConstrs
) where 

import Common (Kind)
import Syntax.Typed.Types (Ty)

import Prelude (class Show, show, (<>),(<$>))
import Data.List (List,intercalate)

data Constr = 
  MkTyEq Ty Ty
  | MkKindEq Kind Kind
  | MkKindNeq Kind Kind
instance Show Constr where 
  show (MkTyEq ty1 ty2) = show ty1 <> " = " <> show ty2
  show (MkKindEq knd1 knd2) = show knd1 <> " = " <> show knd2
  show (MkKindNeq knd1 knd2) = show knd1 <> "!="<> show knd2

type ConstraintSet = List Constr

showConstrs :: ConstraintSet -> String 
showConstrs cs = intercalate "\n\t" (show <$> cs)
