module Syntax.Typed.Terms (
  Command (..),
  Term (..),
  Pattern (..),
  TypedVar,
  getType,
  setType,
  isValue
) where 

import Syntax.Typed.Types 
import Common
import Loc

type TypedVar = (Variable,Ty)

data Command where 
  Cut   :: Loc -> Term -> EvaluationOrder -> Term -> Command 
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

data Pattern = MkPattern{ptxt :: !Xtorname, ptv :: ![Variable], ptcmd :: !Command}
  deriving (Eq)

data Term where 
  Var      :: Loc -> Variable -> Ty -> Term 
  Mu       :: Loc -> Variable -> Command -> Ty -> Term
  Xtor     :: Loc -> Xtorname -> [Term] -> Ty -> Term
  XCase    :: Loc -> [Pattern] -> Ty -> Term 
  ShiftCBV :: Loc -> Term -> Ty -> Term
  ShiftCBN :: Loc -> Term -> Ty -> Term
  deriving (Eq)
instance HasLoc Term where 
  getLoc (Var loc _ _) = loc 
  getLoc (Mu loc _ _ _) = loc
  getLoc (Xtor loc _ _ _) = loc
  getLoc (XCase loc _ _) = loc 
  getLoc (ShiftCBV loc _ _) = loc 
  getLoc (ShiftCBN loc _ _) = loc

  setLoc loc (Var _ v ty) =  Var loc v ty 
  setLoc loc (Mu _ v c ty) = Mu loc v c ty
  setLoc loc (Xtor _ nm args ty) = Xtor loc nm args ty
  setLoc loc (XCase _ pts ty) = XCase loc pts ty 
  setLoc loc (ShiftCBV _ t ty) = ShiftCBV loc t ty 
  setLoc loc (ShiftCBN _ t ty) = ShiftCBN loc t ty

getType :: Term -> Ty
getType (Var _ _ ty)    = ty
getType (Mu _ _ _ ty)   = ty 
getType (Xtor _ _ _ ty) = ty 
getType (XCase _ _ ty)  = ty 
getType (ShiftCBV _ _ ty)  = ty 
getType (ShiftCBN _ _ ty)  = ty 

instance GetKind Term where 
  getKind t = getKind (getType t)

setType :: Term -> Ty -> Term
setType (Var loc v _) ty = Var loc v ty 
setType (Mu loc v c _) ty = Mu loc v c ty
setType (Xtor loc nm args _) ty = Xtor loc nm args ty
setType (XCase loc pts _) ty = XCase loc pts ty
setType (ShiftCBV loc t _) ty = ShiftCBV loc t ty
setType (ShiftCBN loc t _) ty = ShiftCBN loc t ty

isValue :: EvaluationOrder -> Term -> Bool
isValue CBV Var{} = True 
isValue CBV (Xtor _ _ args _) = all (isValue CBV) args
isValue CBV XCase{} = True
isValue CBV ShiftCBV{} = True
isValue CBV _ = False 
isValue CBN _ = True
