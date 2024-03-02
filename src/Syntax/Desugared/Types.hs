module Syntax.Desugared.Types where 

import Common 


data TypeScheme = MkTypeScheme ![PolVar] !Ty 
  deriving (Eq)

data Ty = 
  TyVar !TypeVar
  | TyDecl !TypeName ![Ty] 
  deriving (Eq)
