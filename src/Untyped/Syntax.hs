module Untyped.Syntax where 

import Common 

data Command = Cut !Term !Pol !Term

data Pattern = MkPattern{ptxt :: !String, ptv :: ![Variable], ptcmd :: !Command}

data Term = 
  Var !Variable
  | Mu !Variable !Command 
  -- use constructor type here 
  | Xtor !XtorName ![Term]
  | XCase ![Pattern]
  | Shift !Term
  | Lam !Variable !Command
