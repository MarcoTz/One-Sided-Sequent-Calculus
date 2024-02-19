module Typed.Types where 

import Untyped.Syntax
import Common 

data Ty = 
  TyVar !TypeVar 
  | TyDecl !TypeName ![Ty] 
  | TyShift !Ty
  | TyCo !Ty
  deriving (Eq)

type KindVar = String
data Kind = MkKind !Pol | MkKindVar !KindVar 

data Decl = 
  MkDataDecl{declNm :: !TypeName, declArgs :: ![(Variable,Pol)], declPol :: !Pol, declSig :: ![XtorSig]} 
  | MkValDecl{valVar :: !Variable, valTy :: !Ty, valBd :: !Term}
  | MkRecDecl{recVar :: !Variable, recTy :: !Ty, recBd :: !Term}
  | MkEps
  | MkCoDecl !Decl

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 
