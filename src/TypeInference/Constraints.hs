module TypeInference.Constraints where 

import Syntax.Typed.Types

data Constraint = 
  MkTyEq !Ty !Ty

newtype ConstraintSet = MkConstraintSet { ctrConstr :: [Constraint]}

addConstraint :: Constraint -> ConstraintSet -> ConstraintSet
addConstraint c (MkConstraintSet ctrs) = MkConstraintSet (c:ctrs)
