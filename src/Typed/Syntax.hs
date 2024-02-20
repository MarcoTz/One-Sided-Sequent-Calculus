module Typed.Syntax where 

import Typed.Types 
import Common

data Command = 
  Cut !Term !Pol !Term
  | Done

data Pattern = MkPattern{ptxt :: !String, ptv :: ![Variable], ptcmd :: !Command}

data Term = 
  Var !Variable !Ty
  | Mu !Variable !Command !Ty
  | Xtor !XtorName ![Term] !Ty
  | XCase ![Pattern] !Ty
  | Shift !Term !Ty
  | Lam !Variable !Command !Ty

getType :: Term -> Ty
getType (Var _ ty)    = ty
getType (Mu _ _ ty)   = ty 
getType (Xtor _ _ ty) = ty 
getType (XCase _ ty)  = ty 
getType (Shift _ ty)  = ty 
getType (Lam _ _ ty)  = ty 

class GetKind a where 
  getKind :: a -> Kind 

instance GetKind Ty where 
  getKind (TyVar _ k)    = k
  getKind (TyDecl _ _ k) = k
  getKind (TyShift _ k)  = k 
  getKind (TyCo _ k)     = k 

instance GetKind Term where 
  getKind t = getKind (getType t)
