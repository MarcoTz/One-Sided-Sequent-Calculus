module TwoSided.Types where


type TypeVar = String
type TypeName = String

-- Types 
data Type = TyVar !TypeVar 
  | TyDeclared !TypeName ![Type] 
  | TyDown !Type 
  | TyUp !Type
