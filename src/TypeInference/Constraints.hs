module TypeInference.Constraints where 

import Syntax.Typed.Types
import Common

data Constraint = 
  MkTyEq !Ty !Ty
  | MkKindEq !Kind !Kind
  | MkFlipEq !Kind !Kind

newtype ConstraintSet = MkConstraintSet { ctrConstr :: [Constraint]}

addConstraint :: Constraint -> ConstraintSet -> ConstraintSet
addConstraint c (MkConstraintSet ctrs) = MkConstraintSet (c:ctrs)
