module Constraints (
  ConstraintSet (..),
  Constraint (..),
  insertConstraint
) where 

import Syntax.Typed.Types
import Common

data Constraint = 
  MkTyEq !Ty !Ty
  | MkKindEq !Kind !Kind
  | MkKindNeq !Kind !Kind

newtype ConstraintSet = MkConstraintSet { ctrConstr :: [Constraint]}

insertConstraint :: Constraint -> ConstraintSet -> ConstraintSet
insertConstraint c (MkConstraintSet ctrs) = MkConstraintSet (c:ctrs)
