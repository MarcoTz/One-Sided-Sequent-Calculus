module Syntax.Parsed.Terms (
  Term (..),
  Command (..),
  Pattern (..),
  MTypedVar
) where 

import Common 
import Syntax.Parsed.Types

type MTypedVar = (Variable,Maybe PolTy)
data Command = 
  Cut !Term !Pol !Term
  | CutAnnot !Term !PolTy !Pol !Term
  | Err !String
  | Done
  deriving (Eq,Ord)

data Pattern = MkPattern{ptxt :: !XtorName, ptv :: ![Variable], ptcmd :: !Command}
  deriving (Eq,Ord)

data Term = 
  Var !Variable
  | Mu !Variable !Command 
  | Xtor !XtorName ![Term]
  | XCase ![Pattern]
  | ShiftPos !Term
  | ShiftNeg !Variable !Command
  deriving (Eq,Ord)

