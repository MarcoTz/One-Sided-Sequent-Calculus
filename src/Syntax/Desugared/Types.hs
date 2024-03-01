module Syntax.Desugared.Types where 

import Common 


data TypeScheme = MkTypeScheme ![TypeVar] !Ty 
  deriving (Eq)

data Ty = 
  TyVar !TypeVar
  | TyDecl !TypeName ![Ty] 
  deriving (Eq)
