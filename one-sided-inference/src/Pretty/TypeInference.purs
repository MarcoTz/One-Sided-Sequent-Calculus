module Pretty.TypeInference () where 

import Constraints
import Pretty.Typed ()

import Data.List (intercalate)

instance Show Constraint where 
  show (MkTyEq ty1 ty2) = show ty1 <> " = " <> show ty2
  show (MkKindEq knd1 knd2) = show knd1 <> " = " <> show knd2
  show (MkKindNeq knd1 knd2) = show knd1 <> "!="<> show knd2

instance Show ConstraintSet where 
  show (MkConstraintSet ctrs) = " --- Generated Constraints --- \n\t" <> intercalate "\n\t" (show <$>  ctrs) <> "\n ----------------------------- "
