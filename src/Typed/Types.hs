module Typed.Types where 

import Common 

type KindVar = String
data Kind = MkKind !Pol | MkKindVar !KindVar 
  deriving (Eq)

data Ty = 
  TyVar !TypeVar !Kind
  | TyDecl !TypeName ![Ty] !Kind
  | TyShift !Ty !Kind
  | TyCo !Ty !Kind
  deriving (Eq)
