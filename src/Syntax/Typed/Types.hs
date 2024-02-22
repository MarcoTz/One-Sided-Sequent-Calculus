module Syntax.Typed.Types where 

import Common 


data Ty = 
  TyVar !TypeVar !Kind
  | TyDecl !TypeName ![Ty] !Kind
  | TyShift !Ty !Kind
  | TyCo !Ty !Kind
  deriving (Eq)
