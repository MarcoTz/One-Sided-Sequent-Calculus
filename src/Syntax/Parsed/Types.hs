module Syntax.Parsed.Types where 

import Common


data Ty = 
  TyVar !TypeVar 
  | TyDecl !TypeName ![Ty]
  | TyCo !Ty

type PolTy = (Ty,Pol)
