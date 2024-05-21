module Syntax.Kinded.Terms (
  Command (..),
  Term (..),
  Pattern (..),
  getType,
  setType,
  getPrdCns,
  isValue
) where 

import Syntax.Kinded.Types (Ty)
import Common (EvaluationOrder(..), Xtorname, PrdCns, Variable, class GetKind, getKind)
import Loc (Loc, class HasLoc)

import Prelude (class Eq, ($), identity, class Show, show, (<>), (<$>))
import Data.List (List,null,filter,intercalate)


data Command = 
  Cut     Loc Term EvaluationOrder Term
  | Done  Loc 
  | Err   Loc String 
  | Print Loc Term 
derive instance eqCommand :: Eq Command
instance Show Command where 
  show (Cut _ t eo u) = "<" <> show t <> " | " <> show eo <> " | " <> show u <> ">"
  show (Done _) = "Done"
  show (Err _ msg) = "error " <> show msg
  show (Print _ t) = "Print " <> show t

instance HasLoc Command where 
  getLoc (Cut loc _ _ _) = loc 
  getLoc (Done loc) = loc 
  getLoc (Err loc _) = loc
  getLoc (Print loc _) = loc

  setLoc loc (Cut _ t pol u) = Cut loc t pol u
  setLoc loc (Done _) = Done loc 
  setLoc loc (Err _ str) = Err loc str
  setLoc loc (Print _ t) = Print loc t 

data Pattern = Pattern{ptxt :: Xtorname, ptv :: List Variable, ptcmd :: Command}
derive instance eqPattern :: Eq Pattern
instance Show Pattern where 
  show (Pattern pt) | null pt.ptv = show pt.ptxt <> " => " <> show pt.ptcmd
  show (Pattern pt) = show pt.ptxt <> "(" <> intercalate ", " (show <$> pt.ptv) <> ") =>" <> show pt.ptcmd

data Term = 
  Var        Loc PrdCns Variable Ty 
  | Mu       Loc PrdCns Variable Command Ty
  | Xtor     Loc PrdCns Xtorname (List Term) Ty 
  | XCase    Loc PrdCns (List Pattern) Ty
  | ShiftCBV Loc PrdCns Term Ty 
  | ShiftCBN Loc PrdCns Term Ty 
derive instance eqTerm :: Eq Term
instance Show Term where 
  show (Var _ pc v _)        = show pc <> " " <> show v 
  show (Mu _ pc v c _)       = show pc <> " mu " <> show v <> ". " <> show c 
  show (Xtor _ pc nm args _) | null args = show pc <> " " <>show nm
  show (Xtor _ pc nm args _) = show pc <> " "<> show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (XCase _ pc pts _)    = show pc <> " case { " <> intercalate ", " (show <$> pts) <> " }" 
  show (ShiftCBV _ pc t _)   = show pc <> " {" <> show t <> ":CBV}" 
  show (ShiftCBN _ pc t _)   = show pc <> " {" <> show t <> ":CBV}" 
instance HasLoc Term where 
  getLoc (Var loc _ _ _) = loc 
  getLoc (Mu loc _ _ _ _) = loc
  getLoc (Xtor loc _ _ _ _) = loc
  getLoc (XCase loc _ _ _) = loc 
  getLoc (ShiftCBV loc _ _ _) = loc 
  getLoc (ShiftCBN loc _ _ _) = loc

  setLoc loc (Var _ pc v ty) =  Var loc pc v ty 
  setLoc loc (Mu _ pc v c ty) = Mu loc pc v c ty
  setLoc loc (Xtor _ pc nm args ty) = Xtor loc pc nm args ty
  setLoc loc (XCase _ pc pts ty) = XCase loc pc pts ty 
  setLoc loc (ShiftCBV _ pc t ty) = ShiftCBV loc pc t ty 
  setLoc loc (ShiftCBN _ pc t ty) = ShiftCBN loc pc t ty

getType :: Term -> Ty
getType (Var _ _ _ ty)    = ty
getType (Mu _ _ _ _ ty)   = ty 
getType (Xtor _ _ _ _ ty) = ty 
getType (XCase _ _ _ ty)  = ty 
getType (ShiftCBV _ _ _ ty)  = ty 
getType (ShiftCBN _ _ _ ty)  = ty 

instance GetKind Term where 
  getKind t = getKind (getType t)

setType :: Term -> Ty -> Term
setType (Var loc pc v _) ty = Var loc pc v ty 
setType (Mu loc pc v c _) ty = Mu loc pc v c ty
setType (Xtor loc pc nm args _) ty = Xtor loc pc nm args ty
setType (XCase loc pc pts _) ty = XCase loc pc pts ty
setType (ShiftCBV loc pc t _) ty = ShiftCBV loc pc t ty
setType (ShiftCBN loc pc t _) ty = ShiftCBN loc pc t ty

getPrdCns :: Term -> PrdCns 
getPrdCns (Var _ pc _ _) = pc 
getPrdCns (Mu _ pc _ _ _) = pc 
getPrdCns (Xtor _ pc _ _ _) = pc 
getPrdCns (XCase _ pc _ _) = pc 
getPrdCns (ShiftCBV _ pc _ _) = pc 
getPrdCns (ShiftCBN _ pc _ _) = pc

isValue :: EvaluationOrder -> Term -> Boolean
isValue CBV (Var _ _ _ _) = true 
isValue CBV (Xtor _ _ _ args _) = null $ filter identity ((isValue CBV) <$> args)
isValue CBV (XCase _ _ _ _) = true
isValue CBV (ShiftCBV _ _ _ _) = true
isValue CBV _ = false 
isValue CBN _ = true
isValue Any _ = true
