module Syntax.Typed.Terms (
  Command (..),
  Term (..),
  Pattern (..),
  TypedVar,
  getType,
  isValue
) where 

import Syntax.Typed.Types 
import Common
import Loc

type TypedVar = (Variable,Ty)

data Command where 
  Cut   :: Loc -> Term -> Pol -> Term -> Command 
  Done  :: Loc -> Command 
  Err   :: Loc -> String -> Command 
  Print :: Loc -> Term -> Command
  deriving (Eq)
instance HasLoc Command where 
  getLoc (Cut loc _ _ _) = loc 
  getLoc (Done loc) = loc 
  getLoc (Err loc _) = loc
  getLoc (Print loc _) = loc

  setLoc loc (Cut _ t pol u) = Cut loc t pol u
  setLoc loc (Done _) = Done loc 
  setLoc loc (Err _ str) = Err loc str
  setLoc loc (Print _ t) = Print loc t 

data Pattern = MkPattern{ptxt :: !XtorName, ptv :: ![Variable], ptcmd :: !Command}
  deriving (Eq)

data Term where 
  Var      :: Loc -> Variable -> Ty -> Term 
  Mu       :: Loc -> Variable -> Command -> Ty -> Term
  Xtor     :: Loc -> XtorName -> [Term] -> Ty -> Term
  XCase    :: Loc -> [Pattern] -> Ty -> Term 
  ShiftPos :: Loc -> Term -> Ty -> Term
  ShiftNeg :: Loc -> Variable -> Command -> Ty -> Term
  deriving (Eq)
instance HasLoc Term where 
  getLoc (Var loc _ _) = loc 
  getLoc (Mu loc _ _ _) = loc
  getLoc (Xtor loc _ _ _) = loc
  getLoc (XCase loc _ _) = loc 
  getLoc (ShiftPos loc _ _) = loc 
  getLoc (ShiftNeg loc _ _ _) = loc

  setLoc loc (Var _ v ty) =  Var loc v ty 
  setLoc loc (Mu _ v c ty) = Mu loc v c ty
  setLoc loc (Xtor _ nm args ty) = Xtor loc nm args ty
  setLoc loc (XCase _ pts ty) = XCase loc pts ty 
  setLoc loc (ShiftPos _ t ty) = ShiftPos loc t ty 
  setLoc loc (ShiftNeg _ v c ty) = ShiftNeg loc v c ty

getType :: Term -> Ty
getType (Var _ _ ty)    = ty
getType (Mu _ _ _ ty)   = ty 
getType (Xtor _ _ _ ty) = ty 
getType (XCase _ _ ty)  = ty 
getType (ShiftPos _ _ ty)  = ty 
getType (ShiftNeg _ _ _ ty)  = ty 

instance GetKind Term where 
  getKind t = getKind (getType t)

isValue :: Pol -> Term -> Bool
isValue Pos Var{} = True 
isValue Pos (Xtor _ _ args _) = all (isValue Pos) args
isValue Pos XCase{} = True
isValue Pos ShiftPos{} = True
isValue Pos _ = False 
isValue Neg _ = True
