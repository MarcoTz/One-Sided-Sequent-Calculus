module Syntax where 

data Pol = Pos | Neg 
flipPol :: Pol -> Pol 
flipPol Pos = Neg 
flipPol Neg = Pos 

data Command = Cut !Term !Pol !Term

type Variable = String
data Pattern = MkPattern{ptxt :: !String, ptv :: ![Variable], ptcmd :: !Command}

data Term = 
  Var !Variable
  | Mu !Variable !Command 
  -- use constructor type here 
  | Xtor !String ![Term]
  | XCase ![Pattern]
  | Shift !Term
  | Lam !Variable !Command
