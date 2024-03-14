module Syntax.Parsed.Terms where 

import Common 
import Syntax.Parsed.Types

type MTypedVar = (Variable,Maybe PolTy)
data Command = 
  Cut !Term !Pol !Term
  | CutAnnot !Term !PolTy !Pol !Term
  | Done

data Pattern = MkPattern{ptxt :: !XtorName, ptv :: ![Variable], ptcmd :: !Command}

data Term = 
  Var !Variable
  | Mu !Variable !Command 
  | Xtor !XtorName ![Term]
  | XCase ![Pattern]
  | ShiftPos !Term
  | ShiftNeg !Variable !Command
