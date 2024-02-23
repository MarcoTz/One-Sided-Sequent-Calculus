module Syntax.Typed.Types where 

import Common 

data Ty = 
  TyVar !TypeVar 
  | TyDecl !TypeName ![Ty]
  | TyShift !Ty
  | TyCo !Ty 
  deriving (Eq)
