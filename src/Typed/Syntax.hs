module Typed.Syntax where 

import Typed.Types 
import Common

data Command = Cut !Term !Pol !Term

data Pattern = MkPattern{ptxt :: !String, ptv :: ![Variable], ptcmd :: !Command}

data Term = 
  Var !Variable !Ty
  | Mu !Variable !Command !Ty
  | Xtor !XtorName ![Term] !Ty
  | XCase ![Pattern] !Ty
  | Shift !Term !Ty
  | Lam !Variable !Command !Ty
