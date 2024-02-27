module Syntax.Typed.Terms where 

import Syntax.Typed.Types 
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

instance GetKind Term where 
  getKind t = getKind (getType t)
