module Syntax.Parsed.Terms (
  Term (..),
  Command (..),
  Pattern (..),
  MTypedVar
) where 

import Common 
import Loc
import Syntax.Parsed.Types

type MTypedVar = (Variable,Maybe PolTy)
data Command where 
  Cut        :: Loc -> Term -> Pol -> Term -> Command
  CutAnnot   :: Loc -> Term -> PolTy -> Pol -> Term -> Command
  Err        :: Loc -> String -> Command
  Print      :: Loc -> Term -> Command
  PrintAnnot :: Loc -> Term -> PolTy -> Command
  Done       :: Loc -> Command
  deriving (Eq,Ord)

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

data Pattern = MkPattern{ptxt :: !XtorName, ptv :: ![Variable], ptcmd :: !Command}
  deriving (Eq,Ord)

data Term where 
  Var       :: Loc -> Variable -> Term
  Mu        :: Loc -> Variable -> Command -> Term
  Xtor      :: Loc -> XtorName -> [Term] -> Term
  XCase     :: Loc -> [Pattern] -> Term
  ShiftPos  :: Loc -> Term -> Term
  ShiftNeg  :: Loc -> Variable -> Command -> Term
  deriving (Eq,Ord)
instance HasLoc Term where 
  getLoc (Var loc _) = loc 
  getLoc (Mu loc _ _) = loc 
  getLoc (Xtor loc _ _) = loc 
  getLoc (XCase loc _) = loc 
  getLoc (ShiftPos loc _) = loc 
  getLoc (ShiftNeg loc _ _) = loc

  setLoc loc (Var _ v) = Var loc v 
  setLoc loc (Mu _ v c) = Mu loc v c
  setLoc loc (Xtor _ nm args) = Xtor loc nm args
  setLoc loc (XCase _ pts) = XCase loc pts 
  setLoc loc (ShiftPos _ t) = ShiftPos loc t
  setLoc loc (ShiftNeg _ v c) = ShiftNeg loc v c
