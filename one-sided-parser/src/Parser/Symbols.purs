module Parser.Symbols (Sym (..)) where 

import Prelude (class Show)

data Sym = 
  SymParensO
  | SymParensC 
  | SymComma
  | SymPlus
  | SymMinus
  | SymColon
  | SymBrackO
  | SymBrackC
  | SymLambda
  | SymSqBrackO
  | SymSqBrackC
  | SymEq
  | SymSemi 
  | SymAngO
  | SymAngOUnicode
  | SymAngC
  | SymAngCUnicode
  | SymBar
  | SymDot
  | SymQuot
  | SymBackSl

instance Show Sym where 
  show SymParensO = "("
  show SymParensC = ")"
  show SymComma   = ","
  show SymPlus    = "+"
  show SymMinus   = "-"
  show SymColon   = ":"
  show SymBrackO  = "{"
  show SymBrackC  = "}"
  show SymSqBrackO = "["
  show SymSqBrackC = "]"
  show SymLambda  = "λ"
  show SymEq      = "="
  show SymSemi    = ";"
  show SymAngO    = "<"
  show SymAngOUnicode = "〈"
  show SymAngCUnicode = "〉"
  show SymAngC    = ">"
  show SymBar     = "|"
  show SymDot     = "."
  show SymQuot    = "\""
  show SymBackSl  = "\\"

