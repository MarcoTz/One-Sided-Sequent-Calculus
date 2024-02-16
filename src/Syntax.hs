module Syntax where 

data Pol = Pos | Neg 
  deriving (Eq)
flipPol :: Pol -> Pol 
flipPol Pos = Neg 
flipPol Neg = Pos 

multPol :: Pol -> Pol -> Pol 
multPol Pos Pos = Pos 
multPol Pos Neg = Neg 
multPol Neg Pos = Neg 
multPol Neg Neg = Pos

data Command = Cut !Term !Pol !Term

type Variable = String
data Pattern = MkPattern{ptxt :: !String, ptv :: ![Variable], ptcmd :: !Command}
type XtorName = String

data Term = 
  Var !Variable
  | Mu !Variable !Command 
  -- use constructor type here 
  | Xtor !XtorName ![Term]
  | XCase ![Pattern]
  | Shift !Term
  | Lam !Variable !Command
