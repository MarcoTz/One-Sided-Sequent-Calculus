module Syntax.Desugared.Types where 

import Common 
data Ty = 
  TyVar !TypeVar !Kind
  | TyDecl !TypeName ![Ty] !Kind
