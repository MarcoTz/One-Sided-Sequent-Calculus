module Syntax.Parsed.Terms (
  Term (..),
  Command (..),
  Pattern (..)
) where 

import Common (EvaluationOrder, Xtorname, Variable)
import Loc (Loc, class HasLoc)
import Syntax.Parsed.Types (Ty)

import Prelude (class Eq, class Ord, class Show, show, (<>), (<$>))
import Data.List (List, intercalate, null)

data Command = 
  Cut          Loc Term EvaluationOrder Term 
  | CutAnnot   Loc Term Ty EvaluationOrder Term 
  | Err        Loc String
  | Print      Loc Term 
  | PrintAnnot Loc Term Ty 
  | Done       Loc 
derive instance eqCommand :: Eq Command 
derive instance ordCOmmand :: Ord Command
instance Show Command where 
  show (Cut _ t pol u) = "〈" <> show t <> " | " <> show pol <> " | " <> show u <> "〉"
  show (CutAnnot _ t ty pol u) = "〈" <> show t <> " | " <> show ty <> " | " <> show pol <> " | " <> show u <> "〉" 
  show (Done _) = "Done"
  show (Err _ err) = "error " <> err
  show (Print _ t) = "Print " <> show t
  show (PrintAnnot _ t ty) = " Print " <> show t <> " :: " <> show ty
instance HasLoc Command where 
  getLoc (Cut loc _ _ _ ) = loc 
  getLoc (CutAnnot loc _ _ _ _) = loc 
  getLoc (Err loc _) = loc 
  getLoc (Done loc) = loc
  getLoc (Print loc _) = loc
  getLoc (PrintAnnot loc _ _) = loc

  setLoc loc (Cut _ t pol u) = Cut loc t pol u
  setLoc loc (CutAnnot _ t ty pol u) = CutAnnot loc t ty pol u
  setLoc loc (Err _ str) = Err loc str 
  setLoc loc (Done _) = Done loc
  setLoc loc (Print _ t) = Print loc t 
  setLoc loc (PrintAnnot _ t ty) = PrintAnnot loc t ty

data Pattern = Pattern{ptxt :: Xtorname, ptv :: List Variable, ptcmd :: Command}
derive instance eqPattern :: Eq Pattern
derive instance ordPattern :: Ord Pattern
instance Show Pattern where 
  show (Pattern pt) | null pt.ptv = show pt.ptxt <> " => " <> show pt.ptcmd
  show (Pattern pt) = show pt.ptxt <> "(" <> intercalate ", " (show <$> pt.ptv) <> ") => " <> show pt.ptcmd

data Term =
  Var         Loc Variable 
  | Mu        Loc Variable Command 
  | Xtor      Loc Xtorname (List Term)
  | XCase     Loc (List Pattern) 
  | ShiftCBV  Loc Term 
  | ShiftCBN  Loc Term 
  -- sugar
  | App       Loc Term Term 
  | Lam       Loc Variable Term
  | Seq       Loc Term Term
  | Tup       Loc (List Term)
  | Lst       Loc (List Term)
  | NotBool   Loc Term 
  | AndBool   Loc Term Term
  | OrBool    Loc Term Term

derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term
instance Show Term where 
  show (Var _ v) = show v
  show (Mu _ v cmd) = "mu " <> show v <> ". " <> show cmd
  show (Xtor _ xt args) | null args = show xt
  show (Xtor _ xt args) = show xt <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (XCase _ pts) = "case {" <>  intercalate ", " (show <$> pts) <> "}"
  show (ShiftCBV _ t) = "{" <> show t <> ":CBV}"
  show (ShiftCBN _ t) = "{" <> show t <> ":CBN}" 
  show (App _ t1 t2) = show t1 <> " [" <> show t2 <> "]"
  show (Lam _ v t) = "\\" <> show v <> ". " <> show t
  show (Seq _ t1 t2) = show t1 <> "; " <> show t2
  show (Tup _ ts) = "(" <> intercalate ", " (show <$> ts) <> ")"
  show (Lst _ ts) = "[" <> intercalate ", " (show <$> ts) <> "]"
  show (NotBool _ t) = "!" <> show t
  show (AndBool _ t1 t2) = show t1 <> "&&" <> show t2 
  show (OrBool _ t1 t2) = show t1 <> "||" <> show t2

instance HasLoc Term where 
  getLoc (Var loc _) = loc 
  getLoc (Mu loc _ _) = loc 
  getLoc (Xtor loc _ _) = loc 
  getLoc (XCase loc _) = loc 
  getLoc (ShiftCBV loc _) = loc 
  getLoc (ShiftCBN loc _) = loc
  getLoc (App loc _ _) = loc
  getLoc (Lam loc _ _) = loc
  getLoc (Seq loc _ _) = loc
  getLoc (Tup loc _) = loc
  getLoc (Lst loc _) = loc
  getLoc (NotBool loc _) = loc
  getLoc (AndBool loc _ _) = loc
  getLoc (OrBool loc _ _) = loc

  setLoc loc (Var _ v) = Var loc v 
  setLoc loc (Mu _ v c) = Mu loc v c
  setLoc loc (Xtor _ nm args) = Xtor loc nm args
  setLoc loc (XCase _ pts) = XCase loc pts 
  setLoc loc (ShiftCBV _ t) = ShiftCBV loc t
  setLoc loc (ShiftCBN _ t) = ShiftCBN loc t
  setLoc loc (App _ t1 t2) = App loc t1 t2
  setLoc loc (Lam _ v t) = Lam loc v t
  setLoc loc (Seq _ t1 t2) = Seq loc t1 t2
  setLoc loc (Tup _ ts) = Tup loc ts
  setLoc loc (Lst _ ts) = Lst loc ts
  setLoc loc (NotBool _ t) = NotBool loc t 
  setLoc loc (AndBool _ t1 t2) = AndBool loc t1 t2
  setLoc loc (OrBool _ t1 t2) = OrBool loc t1 t2
