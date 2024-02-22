module Pretty.TypeInference where 

import TypeInference.Constraints
import Pretty.Types ()

instance Show Constraint where 
  show (MkTyEq ty1 ty2) = show ty1 <> " = " <> show ty2
  show (MkKindEq k1 k2) = show k1 <> " = " <> show k2 
  show (MkFlipEq k1 k2) = show k1 <> " != " <> show k2
  show (MkProdEq p k1 k2) = show p <> " * " <> show k1 <> " = " <> show k2
