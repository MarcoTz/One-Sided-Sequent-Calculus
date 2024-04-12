module Syntax.Desugared.Terms (
  Command (..),
  Term (..),
  Pattern (..)
) where 

import Loc (Loc, class HasLoc) 
import Common (EvaluationOrder, Xtorname, Variable) 
import Syntax.Desugared.Types (Ty)

import Prelude (class Show, show, (<>), (<$>), class Eq)
import Data.List (List, intercalate, null) 

data Command =
  Cut          Loc Term  EvaluationOrder  Term 
  | CutAnnot   Loc Term Ty EvaluationOrder Term 
  | Done       Loc 
  | Err        Loc String 
  | Print      Loc Term 
  | PrintAnnot Loc Term Ty 
derive instance eqCommand :: Eq Command
instance Show Command where 
  show (Cut _ t eo u) = "<" <> show t <> " | " <> show eo <> " | " <> show u <> " >"
  show (CutAnnot _ t ty eo u) = "<" <> show t <> " | " <>  show ty <> " : " <> show eo <> " | " <> show u
  show (Done _) = "Done"
  show (Err _ msg) = "errror " <> show msg
  show (Print _ t) = "Print " <> show t
  show (PrintAnnot _ t ty) = "Print " <> show t <> " : " <> show ty

instance HasLoc Command where 
  getLoc (Cut loc _ _ _) = loc 
  getLoc (CutAnnot loc _ _ _ _) = loc 
  getLoc (Done loc) = loc 
  getLoc (Err loc _) = loc
  getLoc (Print loc _) = loc
  getLoc (PrintAnnot loc _ _) = loc

  setLoc loc (Cut _ t pol u) =  Cut loc t pol u
  setLoc loc (CutAnnot _ t ty pol u) = CutAnnot loc t ty pol u
  setLoc loc (Done _) = Done loc 
  setLoc loc (Err _ str) = Err loc str
  setLoc loc (Print _ t) = Print loc t
  setLoc loc (PrintAnnot _ t ty) = PrintAnnot loc t ty

data Pattern = Pattern{ptxt :: Xtorname, ptv :: List(Variable), ptcmd :: Command}
derive instance eqPattern :: Eq Pattern
instance Show Pattern where
  show (Pattern pt) | null pt.ptv = show pt.ptxt <> "=>" <>  show pt.ptcmd
  show (Pattern pt) = show pt.ptxt <> "(" <> intercalate ", " (show <$> pt.ptv) <> ") => " <> show pt.ptcmd

data Term = 
  Var      Loc Variable 
  | Mu       Loc Variable Command 
  | Xtor     Loc Xtorname (List Term)
  | XCase    Loc (List Pattern)
  | ShiftCBV Loc Term
  | ShiftCBN Loc Term
derive instance eqTerm :: Eq Term
instance Show Term where 
  show (Var _ v) = show v 
  show (Mu _ v c) = "mu " <> show v <> ". " <> show c 
  show (Xtor _ nm args) | null args = show nm
  show (Xtor _ nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (XCase _ pts) = "case " <> intercalate ", " (show <$> pts)
  show (ShiftCBV _ t) = "{" <> show t <> ":CBV}"
  show (ShiftCBN _ t) = "{" <> show t <> ":CBN}"


instance HasLoc Term where 
  getLoc (Var loc _) = loc 
  getLoc (Mu loc _ _) = loc
  getLoc (Xtor loc _ _) = loc
  getLoc (XCase loc _) = loc
  getLoc (ShiftCBV loc _) = loc
  getLoc (ShiftCBN loc _) = loc

  setLoc loc (Var _ v) = Var loc v 
  setLoc loc (Mu _ v c) = Mu loc v c
  setLoc loc (Xtor _ nm args) = Xtor loc nm args
  setLoc loc (XCase _ pts) = XCase loc pts
  setLoc loc (ShiftCBV _ t) = ShiftCBV loc t 
  setLoc loc (ShiftCBN _ t) = ShiftCBN loc t
