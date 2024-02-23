module Pretty.TypeInference where 

import TypeInference.Constraints
import Pretty.Types ()

import Data.List (intercalate)

instance Show Constraint where 
  show (MkTyEq ty1 ty2) = show ty1 <> " = " <> show ty2
  show (MkKindEq k1 k2) = show k1 <> " = " <> show k2 
  show (MkFlipEq k1 k2) = show k1 <> " != " <> show k2

instance Show ConstraintSet where 
  show (MkConstraintSet ctrs) = " --- Generated Constraints --- \n\t" <> intercalate "\n\t" (show <$>  ctrs) <> "\n ----------------------------- "
