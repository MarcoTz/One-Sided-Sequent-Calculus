module Syntax.Kinded.Terms (
  Command (..),
  Term (..),
  Pattern (..),
  getType,
  setType,
  isValue
) where 

import Syntax.Kinded.Types (Ty)
import Common (EvaluationOrder(..), Xtorname, Variable, class GetKind, getKind)
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
  Var        Loc Variable Ty 
  | Mu       Loc Variable Command Ty
  | Xtor     Loc Xtorname (List Term) Ty 
  | XCase    Loc (List Pattern) Ty
  | ShiftCBV Loc Term Ty 
  | ShiftCBN Loc Term Ty 
derive instance eqTerm :: Eq Term
instance Show Term where 
  show (Var _ v ty) = show v <> " : " <> show ty
  show (Mu _ v c ty) = show v <> ", " <> show c <> " : " <> show ty
  show (Xtor _ nm args ty) | null args = show nm <> " : " <> show ty
  show (Xtor _ nm args ty) = show nm <> "(" <> intercalate ", " (show <$> args) <> ") : " <> show ty
  show (XCase _ pts ty) = "case { " <> intercalate ", " (show <$> pts) <> " }" <> ": " <> show ty
  show (ShiftCBV _ t ty) = "{" <> show t <> ":CBV} : " <> show ty
  show (ShiftCBN _ t ty) = "{" <> show t <> ":CBV} : " <> show ty

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

isValue :: EvaluationOrder -> Term -> Boolean
isValue CBV (Var _ _ _) = true 
isValue CBV (Xtor _ _ args _) = null $ filter identity ((isValue CBV) <$> args)
isValue CBV (XCase _ _ _) = true
isValue CBV (ShiftCBV _ _ _) = true
isValue CBV _ = false 
isValue CBN _ = true
