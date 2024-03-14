module Syntax.Typed.Terms where 

import Syntax.Typed.Types 
import Common

type TypedVar = (Variable,Ty)

data Command = 
  Cut !Term !Pol !Term
  | Done

data Pattern = MkPattern{ptxt :: !XtorName, ptv :: ![Variable], ptcmd :: !Command}

data Term = 
  Var !Variable !Ty
  | Mu !Variable !Command !Ty
  | Xtor !XtorName ![Term] !Ty
  | XCase ![Pattern] !Ty
  | ShiftPos !Term !Ty
  | ShiftNeg !Variable !Command !Ty

getType :: Term -> Ty
getType (Var _ ty)    = ty
getType (Mu _ _ ty)   = ty 
getType (Xtor _ _ ty) = ty 
getType (XCase _ ty)  = ty 
getType (ShiftPos _ ty)  = ty 
getType (ShiftNeg _ _ ty)  = ty 

instance GetKind Term where 
  getKind t = getKind (getType t)

isValue :: Pol -> Term -> Bool
isValue Pos (Var _ _ ) = True 
isValue Pos (Xtor _ args _) = all (isValue Pos) args
isValue Pos (XCase _ _) = True
isValue Pos (ShiftPos _ _) = True
isValue Pos _ = False 
isValue Neg _ = True
