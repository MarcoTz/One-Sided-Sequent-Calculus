module Syntax.Desugared.Types where 

import Common 


data TypeScheme = MkTypeScheme ![TypeVar] !Ty
data Ty = 
  TyVar !TypeVar
  | TyDecl !TypeName ![Ty] 
