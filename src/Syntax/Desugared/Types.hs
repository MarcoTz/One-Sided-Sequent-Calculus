module Syntax.Desugared.Types where 

import Common 


data Ty = 
  TyVar !TypeVar
  | TyDecl !TypeName ![Ty] 
  | TyCo !Ty
--  | TyForall ![PolVar] !Ty
  deriving (Eq)
