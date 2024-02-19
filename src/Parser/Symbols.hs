module Parser.Symbols where 

data Sym = 
  SymParensO
  | SymParensC 
  | SymComma
  | SymPlus
  | SymMinus
  | SymColon
  | SymBrackO
  | SymBrackC

instance Show Sym where 
  show SymParensO = "("
  show SymParensC = ")"
  show SymComma   = ","
  show SymPlus    = "+"
  show SymMinus   = "="
  show SymColon   = ":"
  show SymBrackO  = "{"
  show SymBrackC  = "}"
