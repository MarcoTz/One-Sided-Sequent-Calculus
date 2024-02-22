module TypeInference.Constraints where 

import Syntax.Typed.Types
import Common

data Constraint = 
  MkTyEq !Ty !Ty
  | MkKindEq !Kind !Kind
  | MkFlipEq !Kind !Kind
  | MkProdEq !Pol !Kind !Kind
