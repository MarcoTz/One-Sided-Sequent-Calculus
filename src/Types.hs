module Types where 

type TypeVar = String
type TypeName = String 
data Ty = 
  TyVar !TypeVar 
  | TyDecl !TypeName ![Ty] 
  | TyShift !Ty
  | TyCo !Ty
