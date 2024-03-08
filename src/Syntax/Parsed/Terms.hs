module Syntax.Parsed.Terms where 

import Common 
import Syntax.Parsed.Types

data Command = 
  Cut !Term !Pol !Term
  | CutAnnot !Term !Ty !Pol !Term
  | Done

data Pattern = MkPattern{ptxt :: !XtorName, ptv :: ![Variable], ptcmd :: !Command}

data Term = 
  Var !Variable
  | Mu !Variable !(Maybe Pol) !Command 
  | Xtor !XtorName ![Term]
  | XCase ![Pattern]
  | ShiftPos !Term
  | ShiftNeg !Variable !Command
