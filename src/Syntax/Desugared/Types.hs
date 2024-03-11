module Syntax.Desugared.Types where 

import Common 


data Ty = 
  TyVar !TypeVar
  | TyDecl !TypeName ![Ty] 
  | TyCo !Ty
  | TyForall ![TypeVar] !Ty
  deriving (Eq)

type PolTy = (Ty,Pol)
