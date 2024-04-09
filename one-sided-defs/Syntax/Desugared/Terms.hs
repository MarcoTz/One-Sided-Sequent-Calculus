module Syntax.Desugared.Terms (
  Command (..),
  Term (..),
  Pattern (..),
) where 

import Common 
import Loc
import Syntax.Desugared.Types

data Command where 
  Cut        :: Loc -> Term -> EvaluationOrder -> Term -> Command 
  CutAnnot   :: Loc -> Term -> Ty -> EvaluationOrder -> Term -> Command 
  Done       :: Loc -> Command 
  Err        :: Loc -> String -> Command 
  Print      :: Loc -> Term -> Command
  PrintAnnot :: Loc -> Term -> Ty -> Command

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

data Pattern = MkPattern{ptxt :: !Xtorname, ptv :: ![Variable], ptcmd :: !Command}

data Term where 
  Var      :: Loc -> Variable -> Term 
  Mu       :: Loc -> Variable -> Command -> Term 
  Xtor     :: Loc -> Xtorname -> [Term] -> Term
  XCase    :: Loc -> [Pattern] -> Term
  ShiftCBV :: Loc -> Term -> Term
  ShiftCBN :: Loc -> Term -> Term

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
